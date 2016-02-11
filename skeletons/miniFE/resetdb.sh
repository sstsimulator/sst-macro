#!/bin/sh
# example script for resetting the eiger logging database

mysql -u root -proot << EOF
drop database $1;
create database $1 ;
use $1
source /home/eric/eiger/database/schema.sql;
quit
EOF
