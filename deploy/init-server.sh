#!/bin/sh

NAME=bannerstalker
PIDFILE=/var/run/$NAME.pid
DAEMON=/home/ubuntu/bannerstalker/bannerstalker
DAEMON_OPTS="--chdir /home/ubuntu/bannerstalker"

start() {
    start-stop-daemon --start --quiet --pidfile $PIDFILE \
        --make-pidfile --background --exec $DAEMON $DAEMON_OPTS
}

stop() {
    start-stop-daemon --stop --quiet --pidfile $PIDFILE --exec $DAEMON
}

case "$1" in
    start)
        echo "Starting $NAME"
        start
        ;;
    stop)
        echo "Stopping $NAME"
        stop
        rm $PIDFILE
        ;;
    restart)
        echo "Restart $NAME"
        stop
        start
        ;;
	*)
		echo "Usage: $NAME {start|stop|restart}" >&2
        exit 2
        ;;
esac

exit 0
