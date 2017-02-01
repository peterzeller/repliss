FROM ubuntu:xenial
MAINTAINER Peter Zeller

ENV WHY3_VER 0.87.3
ENV WHY3_DL https://gforge.inria.fr/frs/download.php/file/36398/why3-0.87.3.tar.gz

# Install base software
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections \
    && apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -y apt-utils \
    && apt-get install -y gcc \
    && apt-get install -y make \
    && apt-get install -y git \
    && apt-get install -y curl \
    && apt-get install -y ocaml \
    && apt-get install -y pkg-config \
    && apt-get install -y libgmp10 \
    && apt-get install -y libgmp-dev \
    && apt-get install -y libgtksourceview2.0 \
    && apt-get install -y libgtksourceview2.0-dev \
    && apt-get install -y menhir \
    && apt-get install -y libzip-ocaml \
    && apt-get install -y libzip-ocaml-dev \
    && apt-get install -y liblablgtksourceview2-ocaml \
    && apt-get install -y liblablgtksourceview2-ocaml-dev

# install why3
RUN cd /tmp && curl -L $WHY3_DL | tar zx && \
    cd /tmp/why3-$WHY3_VER && \
    ./configure --prefix=/usr/local && \
    make && \
    make install && \
    rm -rf /tmp/why3-$WHY3_VER

# Z3 stuff (stolen from https://github.com/SebastianS90/docker-z3-java/blob/master/Dockerfile)
ENV Z3_VERSION "4.5.0"
RUN apt-get install -y binutils g++ make openjdk-8-jre-headless ant wget

# download, compile and install z3
RUN mkdir /opt/z3 && cd /opt/z3 \
 && wget -qO- https://github.com/Z3Prover/z3/archive/z3-${Z3_VERSION}.tar.gz | tar xz --strip-components=1 \
 && python scripts/mk_make.py \
 && cd build \
 && make \
 && make install

# let why3 find provers
RUN why3 config --detect-provers

# copy repliss jar
ADD target/scala-2.11/repliss-assembly-0.1.jar  /opt/repliss/repliss.jar


VOLUME ["/opt/repliss/model/"]

# start repliss
ENV HOST 0.0.0.0
ENV PORT 8080
EXPOSE $PORT
WORKDIR /opt/repliss/
CMD java -jar repliss.jar --server --host $HOST --port $PORT


# cleanup:
    # apt-get autoremove -y \
    #     m4 \
    #     libc6-dev \
    #     make \
    #     libgmp-dev \
    #     manpages-dev \
    #     gcc \
    #     make \
    #     git \
    #     curl \
    #     pkg-config \
    #     libgtksourceview2.0-dev \
    #     dbus \
    #     libzip-ocaml-dev \
    #     liblablgtksourceview2-ocaml-dev && \
    # apt-get clean && \
    # rm -rf /var/lib/apt/lists/*

