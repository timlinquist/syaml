FROM ubuntu:latest

ARG USER_HOME_DIR="/root"

ENV SCALA_VERSION 2.11.8
ENV SBT_VERSION 0.13.9

# Install gnupg
# RUN \
#   apt-get update && \
#   apt-get install apt-utils gnupg gnupg2 gnupg1 grub-pc --assume-yes

RUN apt-get update && apt-get install -my wget gnupg

# Install Docker
RUN \
  apt-get update && \
  apt-get install apt-transport-https ca-certificates --assume-yes && \
  apt-key adv --keyserver hkp://ha.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D && \
  echo "deb https://apt.dockerproject.org/repo ubuntu-xenial main" | tee /etc/apt/sources.list.d/docker.list && \
  apt-get install linux-image-extra-virtual --assume-yes && \
  apt-get install docker-engine --assume-yes

# Install Java
RUN \
  apt-get install default-jdk --assume-yes

# Install Scala
## Piping curl directly in tar
RUN \
  apt-get install curl --assume-yes && \
  curl -fsL http://downloads.typesafe.com/scala/$SCALA_VERSION/scala-$SCALA_VERSION.tgz | tar xfz - -C /root/ && \
  echo >> /root/.bashrc && \
  echo 'export PATH=~/scala-$SCALA_VERSION/bin:$PATH' >> /root/.bashrc

# Install sbt
RUN \
  curl -L -o sbt-$SBT_VERSION.deb http://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb && \
  dpkg -i sbt-$SBT_VERSION.deb && \
  rm sbt-$SBT_VERSION.deb && \
  sbt sbtVersion

VOLUME "$USER_HOME_DIR/.sbt"

# Install nodejs
RUN \
  curl -sL https://deb.nodesource.com/setup_8.x | bash -

RUN \
  apt-get install nodejs --assume-yes