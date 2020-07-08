#!/bin/bash

archUpdates=$(checkupdates | wc -l)
echo "$archUpdates"
