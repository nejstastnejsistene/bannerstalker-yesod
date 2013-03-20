#!/bin/sh

NAME=bannerstalker
PIDFILE=/var/run/$NAME.pid
DAEMON=/home/ubuntu/bannerstalker/bannerstalker
DAEMON_OPTS="--chdir /home/ubuntu/bannerstalker"

start() {
    echo "Starting $NAME"
    start-stop-daemon --start --pidfile $PIDFILE \
        --make-pidfile --background --exec $DAEMON $DAEMON_OPTS
}

stop() {
    echo "Stopping $NAME"
    start-stop-daemon --stop --pidfile $PIDFILE --exec $DAEMON
}

case "$1" in
    start)
        start
        ;;
    stop)
        stop
        rm $PIDFILE
        ;;
    restart)
        stop
        start
        ;;
	*)
		echo "Usage: $NAME {start|stop|restart}" >&2
        exit 2
        ;;
esac

exit 0
