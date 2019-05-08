#!/usr/bin/env bash

echo "Begin docker test"

make gen

if [[ $? != 0 ]]; then
	echo -e "Make gen failed"
	exit 1
fi

make test

if [[ $? != 0 ]]; then
	echo -e "Make test failed"
	exit 1
fi