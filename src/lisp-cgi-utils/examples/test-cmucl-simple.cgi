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
# version: $Id: test-cmucl-simple.cgi 1517 2006-10-26 19:44:51Z als $
#

exec lisp -batch -quiet -load simple.lisp

