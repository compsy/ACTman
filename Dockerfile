# How to test using OpenCPU:
#   1. docker build -t ACTman .
#   2. docker run -p 80:80 ACTman
#   3. Go to <docker IP>/ocpu/test

FROM roqua/opencpu-base
ADD . /ACTman
WORKDIR /ACTman

RUN Rscript inst/bash/install-package-dependencies.sh
RUN R -e 'library("devtools"); install.packages(build(".", path = "."));'
