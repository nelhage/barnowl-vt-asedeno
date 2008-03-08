use strict;
use warnings;

use inc::Module::Install;

barnowl_module('VT_ASedeno');

requires('HTML::WikiConverter');
requires('HTML::WikiConverter::Markdown');
requires('WWW::Mechanize');

WriteAll;
