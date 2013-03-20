default:
	cabal install

devel:
	dist/build/bannerstalker/bannerstalker Development

define copy-init
rm -f /etc/init.d/$2
update-rc.d $2 remove
cp $1 /etc/init.d/$2
chmod 555 /etc/init.d/$2
update-rc.d $2 defaults
endef

server-copy-init:
	$(call copy-init,deploy/init-server.sh,bannerstalker)
	$(call copy-init,deploy/add-to-elb.py,add-to-elb)

daemon-copy-init:
	$(call copy-init,deploy/init-daemon.sh,bannerstalkerd)
