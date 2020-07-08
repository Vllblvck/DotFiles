#!/bin/bash

aurUpdates=$(checkupdates-aur | wc -l)
echo "$aurUpdates"
