import http from "node:http";

let cannedResponse = { content: "Mock response", role: "assistant" };
const requests: any[] = [];

const server = http.createServer((req, res) => {
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Headers", "Content-Type,Authorization");
  res.setHeader("Access-Control-Allow-Methods", "GET,POST,OPTIONS");

  if (req.method === "OPTIONS") { res.writeHead(204); res.end(); return; }

  if (req.url === "/health") { res.writeHead(200); res.end("ok"); return; }

  if (req.url === "/mock-proxy/set-response") {
    let body = "";
    req.on("data", chunk => body += chunk);
    req.on("end", () => {
      cannedResponse = JSON.parse(body);
      res.writeHead(200); res.end("ok");
    });
    return;
  }

  if (req.url === "/mock-proxy/requests") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify(requests));
    return;
  }

  // Record request
  let body = "";
  req.on("data", chunk => body += chunk);
  req.on("end", () => {
    requests.push({ url: req.url, method: req.method, body: JSON.parse(body || "{}"), headers: req.headers });
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify(cannedResponse));
  });
});

const port = parseInt(process.argv.find(a => a.startsWith("--port="))?.split("=")[1] || "3002");
server.listen(port, () => console.log(`Mock proxy on :${port}`));
