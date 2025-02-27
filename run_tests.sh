#!/bin/bash
set -e

usage() {
    echo "Usage: $0 [all|syntax|valid|semantic]"
    exit 1
}

CATEGORY=${1:-all}

trap 'rm -f -- *.s *.o' EXIT

case "$CATEGORY" in
    all)
        echo "Running all tests..."
        scala test .
        ;;
    unit)
        echo "Running unit tests..."
        scala test . --test-only "*Unit*"
        ;;
    valid)
        echo "Running valid tests..."
        scala test . --test-only "*Valid*"
        ;;
    syntax)
        echo "Running syntax tests..."
        scala test . --test-only "*Syntax*"
        ;;
    semantic)
        echo "Running semantic tests..."
        scala test . --test-only "*Semantic*"
        ;;
    backend)
        echo "Running back-end tests..."
        scala test . --test-only "*BackEndIntegration*"
        ;;
    *)
        usage
        ;;
esac

echo "Testing complete!"
rm -f -- *.s
