FROM ocaml/opam:alpine-3.17-ocaml-4.14 AS init-opam
RUN sudo apk update && sudo apk upgrade

# installing basic packages
RUN sudo apk add --no-cache wget

FROM init-opam AS ocaml-base
COPY . .
RUN sudo apk add bash bash-doc bash-completion

# FPTAYLOR
RUN opam init --disable-sandboxing \
	&& sudo wget https://github.com/soarlab/FPTaylor/archive/refs/tags/v0.9.3.tar.gz \
	&& sudo tar -xzf v0.9.3.tar.gz
WORKDIR ./FPTaylor-0.9.3
RUN opam install num && eval $(opam env) && sudo make all
WORKDIR ../

# GAPPA
RUN sudo apk add --no-cache boost-dev \
	&& sudo apk add gmp-dev && sudo apk add mpfr-dev \
	&& sudo wget https://gappa.gitlabpages.inria.fr/releases/gappa-1.4.2.tar.gz \
	&& sudo tar -xzf gappa-1.4.2.tar.gz
WORKDIR ./gappa-1.4.2/
RUN sudo ./configure --prefix=/app/local/ \
	&& sudo ./remake && sudo ./remake install
WORKDIR ../

# NUMFUZZ
RUN opam install dune.3.7.0 menhir.20220210 utop \
	&& eval $(opam env) \
	&& sudo dune build \
	&& sudo chmod +x examples
