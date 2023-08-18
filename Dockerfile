# https://hub.docker.com/r/adoptopenjdk/maven-openjdk8
FROM adoptopenjdk/maven-openjdk8:latest

WORKDIR /app

COPY pom.xml ./
COPY src ./src

RUN mvn clean package -U
