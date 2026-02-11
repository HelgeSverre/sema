use futures::StreamExt;

use crate::types::LlmError;

/// Parse an NDJSON stream from a reqwest Response.
/// Each line is a complete JSON object. Calls `line_handler` for each parsed line.
pub async fn parse_ndjson_stream(
    response: reqwest::Response,
    mut line_handler: impl FnMut(&serde_json::Value) -> Result<(), LlmError>,
) -> Result<(), LlmError> {
    let mut stream = response.bytes_stream();
    let mut buffer = String::new();

    while let Some(chunk) = stream.next().await {
        let chunk = chunk.map_err(|e| LlmError::Http(e.to_string()))?;
        buffer.push_str(&String::from_utf8_lossy(&chunk));

        while let Some(newline_pos) = buffer.find('\n') {
            let line = buffer[..newline_pos].trim().to_string();
            buffer = buffer[newline_pos + 1..].to_string();

            if line.is_empty() {
                continue;
            }

            match serde_json::from_str::<serde_json::Value>(&line) {
                Ok(json) => line_handler(&json)?,
                Err(e) => {
                    // Skip malformed lines rather than failing
                    eprintln!("ndjson: skipping malformed line: {e}");
                }
            }
        }
    }

    // Process remaining buffer
    let remaining = buffer.trim();
    if !remaining.is_empty() {
        if let Ok(json) = serde_json::from_str::<serde_json::Value>(remaining) {
            line_handler(&json)?;
        }
    }

    Ok(())
}
