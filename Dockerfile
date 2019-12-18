FROM ubuntu:19.10
MAINTAINER Peter Zeller


# install repliss dependencies
RUN apt-get update -y \
  && apt-get install -y default-jre-headless graphviz \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# copy repliss jar
ADD target/scala-2.13/repliss-assembly-0.1.jar  /opt/repliss/repliss.jar


VOLUME ["/opt/repliss/model/"]

# start repliss
ENV HOST 0.0.0.0
ENV PORT 8080
EXPOSE $PORT
WORKDIR /opt/repliss/
CMD java -jar repliss.jar --server --host $HOST --port $PORT



