#!/bin/sh

PLANT_ROOT=${HOME}/.plant
PLANT_CACHE_ROOT=${PLANT_ROOT}/cache
PLANT_ECL_ROOT=${PLANT_ROOT}/ecl
PLANT_QUICKLISP_ROOT=${PLANT_ROOT}/quicklisp

ECL_VERSION=13.5.1

build_ecl() {
    if [ ! -d ${PLANT_CACHE_ROOT} ]; then
        mkdir -p ${PLANT_CACHE_ROOT}
    fi

    cd ${PLANT_CACHE_ROOT}

    if [ ! -f ${PLANT_CACHE_ROOT}/ecl-${ECL_VERSION} ]; then
        wget http://downloads.sourceforge.net/project/ecls/ecls/13.5/ecl-${ECL_VERSION}.tgz
    fi

    tar zxf ecl-${ECL_VERSION}.tgz && cd ecl-${ECL_VERSION}

    ./configure --prefix=${PLANT_ECL_ROOT} && \
        make && \
        make install

    rm -rf ecl-${ECL_VERSION}

    cd ${PLANT_ROOT}
}

install_quicklisp() {
    wget http://beta.quicklisp.org/quicklisp.lisp
    
    ${PLANT_ECL_ROOT}/bin/ecl -norc -load quicklisp.lisp \
        -eval '(quicklisp-quickstart:install :path #P"~/.plant/quicklisp/")' \
        -eval '(quit)'

    rm quicklisp.lisp
}

#####################################################

if [ ! -d ${PLANT_ECL_ROOT} ]; then
    build_ecl
fi

if [ ! -f ${PLANT_QUICKLISP_ROOT}/setup.lisp ]; then
    install_quicklisp
fi

# We should have everything we need to compile the plant binary
${PLANT_ECL_ROOT}/bin/ecl -norc \
    -load ${PLANT_QUICKLISP_ROOT}/setup.lisp \
    -eval '(ql:quickload (list :cl-json :getopt))' \
    -eval '(compile-file "plant.lisp" :system-p t)' \
    -eval '(c:build-program "plant-bin" :lisp-files (list "plant.o"))' \
    -eval '(quit)'

rm plant.o
