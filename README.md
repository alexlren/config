# Xconfig

It helps me maintaining multiple configurations on multiple machines.
It is a very simple and basic install script.

## Usage

   sudo mv xconfig /opt
   /opt/xconfig/xconfig install zsh
   source ~/.zshrc

### Install a resource configuration

   xconfig install <resource>

### Uninstall a resource configuration

   xconfig uninstall <resource>

### List available resource configuration

   xconfig list

### Setup a new resource

   mkdir /opt/xconfig/resources/my_resource
   cp ~/.myconfig* /opt/xconfig/resources/my_resource

Then add sufix to each configuration file that can be either:
     * .shared
       The file/directory is still maintained under git control after installing it
     * .private
       The file/directory is simply copied

### Pre/Post install scripts

This will be executed before installing the resource config:
   touch /opt/xconfig/resources/my_resource/pre-hook.sh

This will be executed after installing the resource config:
   touch /opt/xconfig/resources/my_resource/post-hook.sh
