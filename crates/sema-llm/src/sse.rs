use futures::StreamExt;

use crate::types::LlmError;

/// Parse an SSE stream from a reqwest Response.
/// Calls `data_handler` for each `data:` line (excluding `[DONE]`).
pub async fn parse_sse_stream(
    response: reqwest::Response,
    mut data_handler: impl FnMut(&str) -> Result<(), LlmError>,
) -> Result<(), LlmError> {
    let mut stream = response.bytes_stream();
    let mut buffer = String::new();

    while let Some(chunk) = stream.next().await {
        let chunk = chunk.map_err(|e| LlmError::Http(e.to_string()))?;
        buffer.push_str(&String::from_utf8_lossy(&chunk));

        // Process complete lines
        while let Some(newline_pos) = buffer.find('\n') {
            let line = buffer[..newline_pos].trim_end_matches('\r').to_string();
            buffer = buffer[newline_pos + 1..].to_string();

            if line.is_empty() {
                continue;
            }

            // Skip comments and non-data lines
            if line.starts_with(':') || line.starts_with("event:") || line.starts_with("id:") {
                continue;
            }

            if let Some(data) = line.strip_prefix("data: ").or_else(|| line.strip_prefix("data:")) {
                let data = data.trim();
                if data == "[DONE]" {
                    return Ok(());
                }
                if !data.is_empty() {
                    data_handler(data)?;
                }
            }
        }
    }

    Ok(())
}
