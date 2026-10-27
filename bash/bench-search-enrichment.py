"""Benchmark skg text search: the time from the request to the one
'search-results' message, which carries the enriched search buffer.
Usage: python3 -I bash/bench-search-enrichment.py PORT RUNS [TERMS]
TERMS defaults to "something". Each search opens its own connection.
Prints timings, hit counts and sizes only (never content)."""
import re, socket, statistics, sys, time

TERMS = sys . argv [ 3 ] if len ( sys . argv ) > 3 else "something"

def read_lp ( sock ):
    header = b""
    while not header . endswith ( b"\r\n\r\n" ):
        c = sock . recv ( 1 )
        if not c: raise EOFError ( "connection closed" )
        header += c
    m = re . search ( rb"Content-Length:\s*(\d+)", header )
    n = int ( m . group ( 1 ))
    buf = bytearray ()
    while len ( buf ) < n:
        chunk = sock . recv ( min ( 1 << 20, n - len ( buf )))
        if not chunk: raise EOFError ( "connection closed mid-body" )
        buf += chunk
    return time . perf_counter (), bytes ( buf ) . decode ( "utf-8" )

def response_type ( sexp ):
    m = re . search ( r"\(\s*\"?response-type\"?\s+\"?([A-Za-z-]+)", sexp )
    return m . group ( 1 ) if m else "?"

def string_field ( sexp, name ):
    """The string literal following '(name ' or '("name" '."""
    # The sexp crate escapes only backslash and double quote, and
    # prints a string with no whitespace or quote as a bare atom.
    m = re . search ( r"\(\s*\"?" + re . escape ( name ) + r"\"?\s+", sexp )
    if not m: return None
    i, out = m . end (), []
    if sexp [ i ] != '"':
        bare = re . match ( r"[^\s()]+", sexp [ i: ] )
        return bare . group ( 0 ) if bare else None
    i += 1
    while True:
        c = sexp [ i ]
        if c == "\\":
            d = sexp [ i + 1 ]
            out . append ( d )
            i += 2
        elif c == '"': return "".join ( out )
        else:
            out . append ( c ); i += 1

def one_search ( port, body, private_choice = None ):
    req = f'((request . "text search") (terms . "{TERMS}")'
    if body: req += ' (body . "true")'
    if private_choice:
        req += f' (overPrivateText-telescopes . "{private_choice}")'
    req += ")"
    s = socket . create_connection ( ( "127.0.0.1", port ), timeout = 60 )
    s . settimeout ( 60 )
    try:
        t0 = time . perf_counter ()
        s . sendall ( req . encode () + b"\n" )
        t, msg = read_lp ( s )
        rt = response_type ( msg )
        if rt == "overPrivateText-telescope-confirmation":
            return { "confirmation": True, "types": [ ( rt, len ( msg )) ] }
        content = string_field ( msg, "content" ) or ""
        s . settimeout ( 0.3 ) # nothing else should follow
        try:
            extra = read_lp ( s ) [ 1 ]
        except ( socket . timeout, EOFError ):
            extra = None
        return {
            "types": [ ( rt, len ( msg )) ] + (
                [ ( response_type ( extra ), len ( extra )) ] if extra else [] ),
            "t1": t - t0,
            "hits": sum ( 1 for l in content . split ( "\n" )
                          if l . startswith ( "* " )),
            "lines": content . count ( "\n" ) + 1,
            "bytes": len ( content . encode ( "utf-8" )) }
    finally:
        s . close ()

def summary ( xs ):
    return ( f"median {statistics.median(xs)*1000:.1f} ms "
             f"(min {min(xs)*1000:.1f}, max {max(xs)*1000:.1f})" )

def main ():
    port, runs = int ( sys . argv [ 1 ] ), int ( sys . argv [ 2 ] )
    for body in ( False, True ):
        label = "title-only" if not body else "title+body"
        results, choice = [], None
        for i in range ( runs ):
            r = one_search ( port, body, choice )
            if r . get ( "confirmation" ):
                print ( f"[{label}] got overPrivateText confirmation;"
                        " retrying with include" )
                choice = "include"
                r = one_search ( port, body, choice )
            print ( f"[{label}] run {i+1}: types={r['types']} "
                    f"t1={r.get('t1')} hits={r.get('hits')} "
                    f"lines={r.get('lines')} bytes={r.get('bytes')}" )
            results . append ( r )
        print ( f"== {label} (terms: {TERMS!r};"
                f" overPrivateText choice: {choice})" )
        print ( f"  t1, request to search-results:"
                f" {summary([ r['t1'] for r in results ])} over {runs} runs" )
        print ( f"  messages per search:"
                f" {sorted(set(len(r['types']) for r in results))}" )
        for k in ( "hits", "lines", "bytes" ):
            print ( f"  {k}: {sorted(set(r.get(k) for r in results))}" )

main ()
