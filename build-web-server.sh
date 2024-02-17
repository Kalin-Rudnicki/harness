
if [[ -z "$BUILD__DOCKER_APP_NAME" ]]; then
  echo "Error: Missing 'BUILD__DOCKER_APP_NAME' env var." >&2
  exit 1
fi
if [[ -z "$BUILD__DOCKER_TAG" ]]; then
  echo "Error: Missing 'BUILD__DOCKER_TAG' env var." >&2
  exit 1
fi

BUILD__DOCKERFILE_SCRIPT="""
FROM ubuntu:22.04

RUN apt-get update && \
    apt-get install -y openjdk-11-jdk && \
    apt-get clean

WORKDIR /app

COPY harness-web-app-template/res res
COPY harness-web-app-template/api/target/artifacts/harness-web-app-template--api--$BUILD__DOCKER_TAG.jar web-server.jar

CMD [\"java\", \"-jar\", \"web-server.jar\", \"--dev\", \"-C=env:HARNESS_CFG\", \"--\", \"server\"]
"""

BUILD__DOCKERFILE_TEMPFILE=$(mktemp --tmpdir=.)
echo "$BUILD__DOCKERFILE_SCRIPT" >> $BUILD__DOCKERFILE_TEMPFILE

export WEB_SERVER_VERSION="$BUILD__DOCKER_TAG"

sbt \
  "harness-web-app-template--ui-web/webComp --full" \
  harness-web-app-template--api/assembly

docker build --file "$BUILD__DOCKERFILE_TEMPFILE" -t "$BUILD__DOCKER_APP_NAME:$BUILD__DOCKER_TAG" .

rm $BUILD__DOCKERFILE_TEMPFILE
