# -*- mode: python; coding: utf-8 -*-
import pkgconfig

conf = pkgconfig.getConf()
pkgconfig.ensure(conf,'libmodbus >= 3','libmodbus-dev')
env = conf.Finish()

env.Append(CCFLAGS = "-Werror -O0 -g -Wall -std=gnu99")
env.ParseConfig('pkg-config --cflags --libs libmodbus')
env.Program('modclient',Glob('src/*.c'))
