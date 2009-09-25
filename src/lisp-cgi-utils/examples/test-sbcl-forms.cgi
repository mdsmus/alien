#!/bin/sh
#
# 
#
# Copyright (C) 2003-2006 Alexander Schreiber
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation;
# version 2 of the License.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the Free
# Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
# author : Alexander Schreiber <als@thangorodrim.de>
# version: $Id: test-sbcl-forms.cgi 1517 2006-10-26 19:44:51Z als $
#

# it barfs like mad if $HOME isn't set - fucking great

export HOME=/

# this is slow like molasses because it compiles _everything_ first
# for normal deployment, one would work with an already compiled image

# exec sbcl --noinform   --end-runtime-options --noprogrammer --load forms.lisp
exec sbcl --noinform  --end-runtime-options --noprint  --load forms.lisp

