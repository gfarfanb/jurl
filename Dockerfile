# https://hub.docker.com/r/adoptopenjdk/maven-openjdk11
FROM adoptopenjdk/maven-openjdk11:latest

WORKDIR /app

COPY pom.xml ./
COPY src ./src

RUN mvn clean install
