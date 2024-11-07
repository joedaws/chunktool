# extract the version from package.yaml
VERSION=$(grep '^version:' package.yaml | sed 's/version: //; s/^[[:space:]]*//; s/[[:space:]]*$//')


# Check if the version was found
if [ -z "$VERSION" ]; then
  echo "Version not found in package.yaml"
  exit 1
fi

# Create a new Git tag
git tag "v$VERSION"

# Push the tag to GitHub
git push origin "v$VERSION"
