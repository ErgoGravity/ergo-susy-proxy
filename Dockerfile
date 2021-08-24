FROM mozilla/sbt:8u181_1.2.7 as builder
ENV DEBIAN_FRONTEND noninteractive

WORKDIR /susy

ADD ["./app", "/susy/proxy/app"]
ADD ["./conf", "/susy/proxy/conf"]
ADD ["./project", "/susy/proxy/project"]
COPY build.sbt /susy/proxy/

WORKDIR /susy/proxy/
RUN sbt assembly
RUN mv `find . -name *susyproxy-*.jar` /susy-proxy.jar
CMD ["java", "-jar", "/susy-proxy.jar"]

FROM openjdk:8-jre-slim
RUN adduser --disabled-password --home /home/ergo/ --uid 9052 --gecos "ErgoPlatform" ergo && \
    install -m 0750 -o ergo -g ergo  -d /home/ergo/susy
COPY --from=builder /susy-proxy.jar /home/ergo/susy-proxy.jar
COPY ./conf/application.conf /home/ergo/susy/application.conf
RUN chown ergo:ergo /home/ergo/susy-proxy.jar
USER ergo
EXPOSE 9000
WORKDIR /home/ergo
ENTRYPOINT java -jar -D"config.file"=susy/application.conf /home/ergo/susy-proxy.jar

