FROM roqua/opencpu-base

WORKDIR /ACTman

RUN apt-get update && apt-get -f install -y openssl libcurl4-openssl-dev curl libxml2-dev libssl-dev libcairo-dev

ADD ./inst/bash/install-package-dependencies.sh /ACTman/inst/bash/install-package-dependencies.sh

RUN ./inst/bash/install-package-dependencies.sh

ADD ./ /ACTman

RUN R CMD INSTALL --no-multiarch --with-keep.source /ACTman
RUN R CMD build /ACTman
