FROM ocaml/opam:alpine-3.17-ocaml-4.14 AS init-opam
RUN sudo apk update && sudo apk upgrade

# installing basic packages
RUN sudo apk add --no-cache wget

FROM init-opam AS ocaml-base
COPY . .
RUN sudo apk add bash bash-doc bash-completion

# FPTAYLOR
RUN opam init --disable-sandboxing 
WORKDIR ./examples/FPTaylor/FPTaylor-0.9.3
RUN opam install num && eval $(opam env) && sudo make all
WORKDIR ../../../

# GAPPA
RUN sudo apk add --no-cache boost-dev \
	&& sudo apk add gmp-dev && sudo apk add mpfr-dev
WORKDIR ./examples/Gappa/gappa-1.4.2/
RUN sudo ./configure --prefix=/app/local/ \
	&& sudo ./remake && sudo ./remake install
WORKDIR ../../../

# NUMFUZZ
RUN opam install dune.3.7.0 menhir.20220210 utop \
	&& eval $(opam env) \
	&& sudo dune build 