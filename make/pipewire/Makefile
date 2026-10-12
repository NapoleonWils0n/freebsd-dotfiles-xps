#===============================================================================
# FILE: Makefile
# Project: Start Pipewire and xdg desktop portals
# for OBS Studio and Browser Screen sharing
#===============================================================================

#===============================================================================
# Required Packages:
# doas pkg install pipewire wireplumber xdg-desktop-portal xdg-desktop-portal-wlr 
#===============================================================================

#===============================================================================
# Usage:
# Start Pipewire = make
# Stop Pipewire = make clean
#===============================================================================

#===============================================================================
# PHONY
#===============================================================================

.PHONY: all clean pipwire_start


#===============================================================================
# commands
#===============================================================================

# pipewire
PIPEWIRE_NAME  	:= pipewire
PIPEWIRE_EXEC  	:= /usr/local/bin/pipewire > /dev/null 2>&1 &

# xdg-desktop-portal
PORTAL_EXEC	:= /usr/local/libexec/xdg-desktop-portal -r > /dev/null 2>&1 &

# xdg-desktop-portal-wlr
PORTAL_WLR_NAME	:= xdg-desktop-portal-wlr
PORTAL_WLR_EXEC	:= /usr/local/libexec/xdg-desktop-portal-wlr -r > /dev/null 2>&1 &

# kill pipewire and xdg-desktop-portal-wlr
KILLALL		:= killall -9 $(PIPEWIRE_NAME) $(PORTAL_WLR_NAME) 2>/dev/null

#===============================================================================
# default target
#===============================================================================

all: pipewire_start


#===============================================================================
# pipewire start
#===============================================================================

pipewire_start: 
	@echo 'Starting Pipewire and XDG Portals'
	@$(PIPEWIRE_EXEC)
	@$(PORTAL_EXEC)
	@$(PORTAL_WLR_EXEC)


#===============================================================================
# clean 
#===============================================================================

clean:
	@echo 'Stopping Pipewire and XDG Portals'
	@$(KILLALL)
