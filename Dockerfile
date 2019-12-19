FROM ubuntu:19.10
MAINTAINER Peter Zeller


# install repliss dependencies
RUN apt-get update -y \
  && apt-get install -y openjdk-14-jre-headless graphviz \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# copy repliss jar
ADD target/scala-2.13/repliss-assembly-0.1.jar  /opt/repliss/repliss.jar


VOLUME ["/opt/repliss/model/"]

# start repliss
WORKDIR /opt/repliss/
ENTRYPOINT ["java", "-jar", "repliss.jar"]



