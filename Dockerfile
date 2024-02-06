FROM maven:3-eclipse-temurin-8

WORKDIR /app

COPY pom.xml ./
COPY src ./src

RUN mvn clean package -U
