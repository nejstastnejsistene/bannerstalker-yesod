#!/usr/bin/env python

import commands
import sys
import boto.ec2.elb


if __name__ == '__main__':
    cmd = 'ec2metadata --instance-id'
    status, instance_id = commands.getstatusoutput(cmd)
    if status:
        print 'Unable to determine instance-id.'
        sys.exit(1) 
    conn = boto.ec2.elb.connect_to_region('us-east-1')
    lb = conn.get_all_load_balancers(['bannerstalker'])[0]
    lb.register_instances([instance_id])
