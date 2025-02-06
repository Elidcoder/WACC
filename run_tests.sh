#!/bin/bash
set -e

# trap SIGTERM and exit gracefully
trap 'echo "Job cancelled - exiting gracefully"; exit 0' SIGTERM

usage() {
    echo "Usage: $0 [all|syntax|valid|semantic]"
    exit 1
}

CATEGORY=${1:-all}

echo "Compiling project..."
scala compile .
echo "Compilation complete!"

case "$CATEGORY" in
    all)
        echo "Running all tests..."
        scala test .
        ;;
    unit)
        echo "Running unit tests..."
        scala test . --test-only "*UnitTest*"
        ;;
    valid)
        echo "Running valid tests..."
        scala test . --test-only "*ValidTest*"
        ;;
    syntax)
        echo "Running syntax tests..."
        scala test . --test-only "*SyntaxTest*"
        ;;
    semantic)
        echo "Running semantic tests..."
        scala test . --test-only "*SemanticTest*"
        ;;
    *)
        usage
        ;;
esac

echo "Testing complete!"
