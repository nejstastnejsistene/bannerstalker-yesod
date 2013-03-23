#!/usr/bin/env python

import boto.ec2
import boto.ec2.elb


if __name__ == '__main__':
    ec2_conn = boto.ec2.connection.EC2Connection()
    instances = {}
    for r in  ec2_conn.get_all_instances():
        for i in r.instances:
            instances[i.id] = i
    elb_conn = boto.ec2.elb.connect_to_region('us-east-1')
    lb = elb_conn.get_all_load_balancers(['bannerstalker'])[0]
    for i in lb.instances:
        print instances[i.id].public_dns_name
