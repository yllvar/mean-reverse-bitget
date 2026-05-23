FROM ocaml/opam:alpine-ocaml-4.14 AS build
WORKDIR /app
COPY --chown=opam . .
RUN opam install cohttp-lwt-unix yojson digestif base64 lwt tls-lwt
RUN opam exec -- dune build

FROM alpine:3.19
RUN apk add --no-cache ca-certificates libgcc gmp
COPY --from=build /app/_build/default/bin/main.exe /app/main.exe
CMD ["/app/main.exe"]
