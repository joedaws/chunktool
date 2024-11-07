#!/bin/bash

set -e

GHC_VERSION=$1

# Check if curl is installed, and install it if not
if ! command -v curl &> /dev/null; then
  echo "curl not found. Installing curl..."
  if [ "$(uname)" == "Linux" ]; then
    sudo apt-get update
    sudo apt-get install -y curl
  elif [ "$(uname)" == "Darwin" ]; then
    brew install curl || echo "Please install Homebrew to proceed."
  elif [ "$(uname)" == "MINGW" ]; then
    echo "Please install curl manually for Windows support."
    exit 1
  fi
else
  echo "curl is already installed."
fi

# Install Stack
echo "Installing Stack..."
curl -sSL https://get.haskellstack.org/ | sh

# Set up Stack to use the specified GHC version
echo "Configuring Stack to use GHC version $GHC_VERSION..."
stack setup $GHC_VERSION

echo "Stack and GHC $GHC_VERSION installed successfully."
