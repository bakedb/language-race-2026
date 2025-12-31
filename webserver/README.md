# Language Race Web Server

This web server provides the test endpoints for the Language Race hash comparison task.

## Setup

1. Install dependencies:
```bash
npm install
```

2. Start the server:
```bash
npm start
```

The server will run on `http://localhost:3000` by default.

## Endpoints

- **Root**: `http://localhost:3000/` - Main page with all test endpoints
- **Test endpoints**: `http://localhost:3000/test-0` through `http://localhost:3000/test-99`

Each test endpoint returns a JSON object with a hash string:
```json
{"hash": "5f4dcc3b5aa765d61d8327deb882cf99"}
```

## Usage

The language programs should:
1. Make HTTP requests to `/test-0` through `/test-99`
2. Extract the hash from each response
3. Compare each hash with the corresponding value in `compare.json`
4. Output the comparison results to `test-result.json`

## compare.json

Contains the expected hash values for all 100 test endpoints. The language programs should load this file and compare the server responses against these values.

## Example Response

```bash
curl http://localhost:3000/test-5
# Returns: {"hash": "b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1"}
```

## Stopping the Server

Press `Ctrl+C` to stop the server gracefully.
