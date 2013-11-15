# Xconfig

Just a simple tool to helps me maintaining multiple configurations on multiple machines.
It is a very simple and basic install script.

## Usage
<pre>
   sudo mv xconfig /opt
   /opt/xconfig/xconfig install zsh
   source ~/.zshrc
</pre>
### Install a resource configuration
<pre>
   xconfig install <resource>
</pre>
### Uninstall a resource configuration
<pre>
   xconfig uninstall <resource>
</pre>
### List available resource configuration
<pre>
   xconfig list
</pre>
### Setup a new resource

* Copy the shared resources
<pre>
   mkdir -p /opt/xconfig/resources/my_resource/share
   cp ~/.myconfig* /opt/xconfig/resources/my_resource/share
</pre>

* Copy the non-shared resources i.e. specific to your environment
<pre>
   mkdir -p /opt/xconfig/resources/my_resource/local
   cp ~/.myconfig* /opt/xconfig/resources/my_resource/local
</pre>

### Pre/Post install scripts

This will be executed before installing the resource config:
<pre>
   touch /opt/xconfig/resources/my_resource/hooks/pre-install.sh
</pre>
This will be executed after installing the resource config:
<pre>
   touch /opt/xconfig/resources/my_resource/hooks/post-install.sh
</pre>
