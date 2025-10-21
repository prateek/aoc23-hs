#!/usr/bin/env node

/**
 * Mock event collector for testing
 * Receives NDJSON events via HTTP POST and stores them in memory
 */

const http = require('http');
const PORT = 8787;

const events = [];

const server = http.createServer((req, res) => {
  // Enable CORS
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization, DD-API-KEY');

  if (req.method === 'OPTIONS') {
    res.writeHead(200);
    res.end();
    return;
  }

  if (req.method === 'POST' && (req.url === '/' || req.url.startsWith('/v1/input') || req.url.startsWith('/loki'))) {
    let body = '';

    req.on('data', (chunk) => {
      body += chunk.toString();
    });

    req.on('end', () => {
      try {
        // Parse NDJSON (one JSON object per line)
        const lines = body.split('\n').filter((line) => line.trim());
        lines.forEach((line) => {
          try {
            const event = JSON.parse(line);
            events.push(event);
            console.log('[mock-collector] Received event:', JSON.stringify(event, null, 2));
          } catch (e) {
            // Try parsing as single JSON array (Datadog format)
            const parsed = JSON.parse(body);
            if (Array.isArray(parsed)) {
              parsed.forEach((evt) => events.push(evt));
              console.log(`[mock-collector] Received ${parsed.length} events`);
            } else {
              events.push(parsed);
              console.log('[mock-collector] Received event:', JSON.stringify(parsed, null, 2));
            }
          }
        });

        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ status: 'ok', received: lines.length }));
      } catch (error) {
        console.error('[mock-collector] Parse error:', error);
        res.writeHead(400, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: 'Invalid JSON' }));
      }
    });

    return;
  }

  if (req.method === 'GET' && req.url === '/events') {
    // Return all collected events
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify(events));
    return;
  }

  if (req.method === 'POST' && req.url === '/clear') {
    // Clear all events
    events.length = 0;
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ status: 'cleared' }));
    return;
  }

  res.writeHead(404);
  res.end('Not found');
});

server.listen(PORT, () => {
  console.log(`[mock-collector] Listening on http://localhost:${PORT}`);
  console.log(`[mock-collector] POST / - receive events (NDJSON)`);
  console.log(`[mock-collector] GET /events - retrieve all events`);
  console.log(`[mock-collector] POST /clear - clear all events`);
});

// Graceful shutdown
process.on('SIGTERM', () => {
  console.log('[mock-collector] Shutting down...');
  server.close(() => {
    process.exit(0);
  });
});
