# Xconfig

Just a simple tool to helps me maintaining multiple configurations on multiple machines.
It is a very simple and basic install script.

## Usage

<pre>
   sudo mv xconfig /opt
   /opt/xconfig/xconfig install zsh
   source ~/.zshrc
</pre>

### Print help

<pre>
   xconfig help [ command ]
</pre>

### Install a resource configuration

<pre>
   xconfig install [ resource ... ]
</pre>

### Uninstall a resource configuration

<pre>
   xconfig uninstall resource [ resource ... ]
</pre>

### List available resource configuration

<pre>
   xconfig list
</pre>

### Update xconfig

<pre>
   xconfig update [ -f ]
</pre>

### Setup a new resource

* Copy the shared resources
<pre>
   mkdir -p /opt/xconfig/resources/my_resource/share
   cp ~/.myconfig* /opt/xconfig/resources/my_resource/share
</pre>

* Copy the local resources i.e. specific to your environment (a post-install hook can also handle it dynamicly)
<pre>
   mkdir -p /opt/xconfig/resources/my_resource/local
   cp ~/.myconfig* /opt/xconfig/resources/my_resource/local
</pre>

### Add pre/post (un)install scripts

This will be executed before installing the resource config:
<pre>
   touch /opt/xconfig/resources/my_resource/hooks/pre-install.sh
</pre>
This will be executed after installing the resource config:
<pre>
   touch /opt/xconfig/resources/my_resource/hooks/post-install.sh
</pre>
This will be executed before uninstalling the resource config:
<pre>
   touch /opt/xconfig/resources/my_resource/hooks/pre-uninstall.sh
</pre>
This will be executed after uninstalling the resource config:
<pre>
   touch /opt/xconfig/resources/my_resource/hooks/post-uninstall.sh
</pre>
