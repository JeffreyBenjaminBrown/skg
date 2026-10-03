#!/usr/bin/env python3
"""Minimal skg TCP client: sends one request line (plus optional
Content-Length body), then prints every length-prefixed response
until the server goes quiet."""
import os, re, socket, sys, time

HOST, PORT = "127.0.0.1", int ( os . environ . get ( "SKG_PORT", "1750" ))

def read_lp_message ( sock, timeout ):
    """Read one 'Content-Length: N\\r\\n\\r\\nPAYLOAD' message.
    Returns payload bytes, or None on timeout/close."""
    sock . settimeout ( timeout )
    buf = b""
    try:
        while b"\r\n\r\n" not in buf:
            chunk = sock . recv ( 1 )
            if not chunk: return None
            buf += chunk
        header = buf . split ( b"\r\n\r\n" )[0] . decode ()
        n = int ( header . split ( ":" )[1] . strip () )
        payload = b""
        while len ( payload ) < n:
            chunk = sock . recv ( n - len ( payload ) )
            if not chunk: return None
            payload += chunk
        return payload
    except socket . timeout:
        return None

def run ( request_line, body = None, expect_multi = False ):
    with socket . create_connection (( HOST, PORT )) as s:
        s . sendall ( request_line . encode () + b"\n" )
        if body is not None:
            b = body . encode ()
            s . sendall ( f"Content-Length: {len(b)}\r\n\r\n" . encode () + b )
        msgs = []
        while True:
            m = read_lp_message ( s, 15 if not msgs else 3 )
            if m is None: break
            msgs . append ( m . decode ( "utf-8", "replace" ))
            if not expect_multi: break
        return msgs

if __name__ == "__main__":
    mode = sys . argv [1]
    if mode == "view":
        node_id, uri = sys . argv [2], sys . argv [3]
        req = f'((request . "single root content view") (id . "{node_id}") (view-uri . "{uri}"))'
        msgs = run ( req )
    elif mode == "save":
        uri, body_file = sys . argv [2], sys . argv [3]
        body = open ( body_file ) . read ()
        req = ( f'((request . "save buffer") (view-uri . "{uri}")'
                ' (point-lines-below-focused-headline . "0")'
                ' (point-column . "0")'
                ' (point-screen-lines-below-window-start . "0"))' )
        msgs = run ( req, body = body, expect_multi = True )
    elif mode == "raw":
        msgs = run ( sys . argv [2], expect_multi = True )
    elif mode == "import":
        # Preview and approval must share one connection: the approval
        # token is bound to the session that previewed.
        input_dir, source = sys . argv [2], sys . argv [3]
        msgs = []
        with socket . create_connection (( HOST, PORT )) as s:
            def send ( fields ):
                line = ( '((request . "import md and org") ' +
                         " " . join ( f'({k} . "{v}")' for k, v in fields ) + ")\n" )
                s . sendall ( line . encode () )
                m = read_lp_message ( s, 600 ) . decode ( "utf-8", "replace" )
                msgs . append ( m )
                return m
            preview = [ ( "action", "preview" ), ( "input-directory", input_dir ),
                        ( "destination-source", source ) ]
            m = send ( preview )
            if "host-mapping-needed" in m:  # blank: leave absolute links unresolved
                m = send ( preview + [ ( "host-root", "" ) ] )
            token = re . search ( r'\(approval-token "?([^" )]+)', m )
            if token:
                send ( [ ( "action", "apply" ), ( "approval-token", token . group (1) ) ] )
    for i, m in enumerate ( msgs ):
        print ( f"--- message {i} ({len(m)} chars) ---" )
        print ( m )
