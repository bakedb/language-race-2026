#!/bin/bash
cd "$(dirname "$0")"
java -cp ".:json-20231013.jar" WebServer "$@"
