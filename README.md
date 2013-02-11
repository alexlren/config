# Xconfig

It helps me maintaining multiple configurations on multiple machines.
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
<pre>
   mkdir /opt/xconfig/resources/my_resource
   cp ~/.myconfig* /opt/xconfig/resources/my_resource
</pre>
Then add sufix to each configuration file that can be either:
* .shared

The file/directory is still maintained under git control after installing it
* .private

The file/directory is simply copied

### Pre/Post install scripts

This will be executed before installing the resource config:
<pre>
   touch /opt/xconfig/resources/my_resource/pre-hook.sh
</pre>
This will be executed after installing the resource config:
<pre>
   touch /opt/xconfig/resources/my_resource/post-hook.sh
</pre>
