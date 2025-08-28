use crate::serve::util::send_response_with_length_prefix;
use std::io::{BufRead, BufReader, Read};
use std::net::TcpStream;

/// Handles save buffer requests from Emacs.
/// Reads the buffer content with length prefix,
/// processes it by prepending a line,
/// and sends it back with length prefix.
pub fn handle_save_buffer_request(
    stream: &mut TcpStream,
    _request: &str,  // The initial request line (not used for this simple case)
) {
    match read_length_prefixed_content(stream) {
        Ok(content) => {
            let processed_content = process_buffer_content(&content);
            send_response_with_length_prefix(stream, &processed_content);
        }
        Err(err) => {
            let error_msg = format!("Error reading buffer content: {}", err);
            println!("{}", error_msg);
            // Send error without length prefix for simplicity
            use crate::serve::util::send_response;
            send_response(stream, &error_msg);
        }
    }
}

/// Reads length-prefixed content from the stream.
/// Expects format: "Content-Length: N\r\n\r\n" followed by N bytes of content.
fn read_length_prefixed_content(stream: &mut TcpStream) -> Result<String, Box<dyn std::error::Error>> {
    let mut reader = BufReader::new(stream);

    // Read the header lines until we get the empty line
    let mut header_lines = Vec::new();
    loop {
        let mut line = String::new();
        reader.read_line(&mut line)?;
        if line == "\r\n" {
            break; // Empty line signals end of headers
        }
        header_lines.push(line);
    }

    // Parse Content-Length from headers
    let content_length = header_lines
        .iter()
        .find_map(|line| {
            if line.starts_with("Content-Length: ") {
                line.strip_prefix("Content-Length: ")
                    .and_then(|s| s.trim().parse::<usize>().ok())
            } else {
                None
            }
        })
        .ok_or("Content-Length header not found")?;

    // Read exactly content_length bytes
    let mut buffer = vec![0u8; content_length];
    reader.read_exact(&mut buffer)?;

    // Convert to UTF-8 string
    let content = String::from_utf8(buffer)?;
    Ok(content)
}

/// Processes the buffer content by prepending a line.
/// This is where the actual "save" logic would go in a real implementation.
fn process_buffer_content(content: &str) -> String {
    format!("Rust added this line.\n{}", content)
}
