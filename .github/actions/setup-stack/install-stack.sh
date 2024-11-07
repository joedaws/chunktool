#!/bin/bash

set -e

GHC_VERSION=$1

# Install Stack
echo "Installing Stack..."
curl -sSL https://get.haskellstack.org/ | sh

# Set up Stack to use the specified GHC version
echo "Configuring Stack to use GHC version $GHC_VERSION..."
stack setup $GHC_VERSION

echo "Stack and GHC $GHC_VERSION installed successfully."
