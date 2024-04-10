FROM ocaml/opam:alpine-3.17-ocaml-4.14 AS init-opam
RUN sudo apk update && sudo apk upgrade

# installing basic packages
RUN sudo apk add --no-cache wget

FROM init-opam AS ocaml-base
COPY . NumFuzz
RUN sudo apk add bash bash-doc bash-completion
WORKDIR NumFuzz

# FPTAYLOR
RUN opam init --disable-sandboxing 
WORKDIR ./examples/FPTaylor
RUN sudo mkdir FPTaylor-0.9.4 \
	&& sudo tar -xzf v0.9.4.tar.gz -C FPTaylor-0.9.4
WORKDIR ./FPTaylor
RUN opam install num && eval $(opam env) && sudo make all
WORKDIR ../../../

# GAPPA
RUN sudo apk add --no-cache boost-dev \
	&& sudo apk add gmp-dev && sudo apk add mpfr-dev
WORKDIR ./examples/Gappa
RUN sudo tar -xzf gappa-1.4.2.tar.gz
WORKDIR ./gappa-1.4.2/
RUN sudo ./configure \
	&& sudo ./remake && sudo ./remake install
WORKDIR ../../../

# NUMFUZZ
RUN opam install dune.3.7.0 menhir.20220210 utop \
	&& eval $(opam env) \
	&& sudo dune build 
