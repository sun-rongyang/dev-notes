#!/bin/sh

yum check-update
curl -fsSL https://get.docker.com/ | sh
systemctl start docker
systemctl enable docker


curl -L "https://github.com/docker/compose/releases/download/1.23.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose


curl -sSLO https://github.com/shadowsocks/shadowsocks-libev/raw/master/docker/alpine/docker-compose.yml


#docker-compose up -d
#docker-compose ps
