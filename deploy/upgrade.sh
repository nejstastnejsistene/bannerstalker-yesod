#!/bin/sh
SSH_OPTS="-i $HOME/.ssh/bannerstalker-key.pem"
LOGIN_NAME=ubuntu
for server in `deploy/list-instances.py`; do
    echo deploying to $server
    ssh $SSH_OPTS $LOGIN_NAME@$server sudo service bannerstalker stop
    ssh $SSH_OPTS $LOGIN_NAME@$server "cd bannerstalker && git fetch origin && git reset --hard origin/master"
    scp $SSH_OPTS dist/build/bannerstalker/bannerstalker $LOGIN_NAME@$server:bannerstalker
    ssh $SSH_OPTS $LOGIN_NAME@$server sudo service bannerstalker start
done
