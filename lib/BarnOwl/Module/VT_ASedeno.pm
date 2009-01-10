use strict;
use warnings;

package BarnOwl::Module::VT_ASedeno;

use BarnOwl::Hooks;

use Text::Autoformat;
use HTML::WikiConverter;
use Encode qw(encode decode);
use Encode::MIME::Header;
use WWW::Mechanize;

*boldify = \&BarnOwl::Style::boldify;

BarnOwl::new_variable_bool('vta:escape_formatting', {
    default => 1,
    summary => "Escape z-formatting in BarnOwl::Style::VT_ASedeno"
   });

################################################################################
#Run this on start and reload. Adds styles, sets style to start.
################################################################################
sub onStart
{
    my $reload = shift;
    init_VT();
}

$BarnOwl::Hooks::startup->add(\&onStart);

################################################################################
# Branching point for various formatting functions in this style.
################################################################################

sub format_time {
    my $m = shift;
    my ($time) = $m->time =~ /(\d\d:\d\d)/;
    return $time;
}

sub format_msg($)
{
    my $m = shift;

    if ($m->is_zephyr)
    {
	return format_VT($m);
    }
    elsif ($m->is_aim)
    {
	return format_VT_AIM($m);
    }
    elsif ($m->is_jabber)
    {
	return format_VT_jabber($m);
    }
    elsif ($m->type eq 'IRC')
    {
        return format_VT_IRC($m);
    }
    elsif (lc $m->type eq 'twitter')
    {
        return format_VT_twitter($m);
    }
    elsif ($m->is_admin)
    {
	return "\@bold(OWL ADMIN):\t".$m->body;
    }
    elsif ($m->is_loopback)
    {
	return "\@bold(loopback):  ".$m->body;
    }
    else
    {
	return $m->type.":\t".$m->body;
    }
}

################################################################################
# A place to keep my options all together, with default values.
################################################################################
our %VT_Options =
    ("zsigs" => 0,
     "showControl" => 0,
     "stripMitEdu" => 1,
     "narrowMode" => 100);

sub init_VT
{
    # Style definition
    owl::command("style VT perl BarnOwl::Module::VT_ASedeno::format_msg");

    # Command aliases
    owl::command("alias VT view -s VT");
}

#Turn zsigs on or off
sub VT_toggle_sigs
{
    $VT_Options{"zsigs"} = !($VT_Options{"zsigs"});
    refreshView();
}

#Toggle stripping of MIT.EDU from hosts
sub VT_toggle_host_strip
{
    $VT_Options{"stripMitEdu"} = !($VT_Options{"stripMitEdu"});
    refreshView();
}

#Toggle literal backspace display method
sub VT_toggle_control
{
    $VT_Options{"showControl"} = !($VT_Options{"showControl"});
    refreshView();
}

sub refreshView()
{
    my $filter = owl::command("getview");
    my $style = owl::command("getstyle");
    owl::command("view -f $filter ".($style?"-s $style":""));
}

sub clean_utf8 {
    my $text = shift;
    eval {
          my $utf = decode('utf-8', $text, 1);
          # $text = unidecode($utf);
          $text = $utf;
      };
    return $text;
}

sub zescape {
    my $text = shift;
    if(BarnOwl::getvar('vta:escape_formatting') eq 'on') {
        $text =~ s/@/@@/g;
    }
    return $text;
}

################################################################################
# Functions to format zephyrs.
# header for large screens (>narrowMode cols):
#  username___.HH:MM.class[instance]___.A. (width: 38)
################################################################################
sub format_VT($)
{
    my $m = shift;

    # Extract time from message
    my $time = format_time($m);

    # Deal with PING messages, assuming owl's rxping variable is true.
    if ($m->is_ping)
    {
        if($m->direction eq 'in') {
            return("\@bold(PING) from \@bold(".$m->pretty_sender.")\n");
        } else {
            return("\@bold(PING) to \@bold(".$m->pretty_recipient.")\n");
        }
    }

    # Deal with login/logout messages
    elsif ($m->is_loginout)
    {
	return sprintf('@b(%-10.10s) %s @b(%s) at %s %s',
		       $m->pretty_sender,
		       $time,
		       uc($m->login),
		       uc($m->host),
		       $m->login_tty);
    }

    # Extract destination from message
    my $dest;

    if ($m->is_personal)
    {
	# Special casing personal zephyrs. Yes, we could use personals as a
	# case of -c message, but I want the consistency of case on display.
	$dest = '[personal]';
    }
    elsif (lc($m->instance) eq 'personal')
    {
	# Since personal is the default instance, strip it and just use the
	# class name.
	$dest = clean_utf8($m->context);
    }
    elsif (lc($m->class) eq 'message')
    {
	# Since message is the default class, strip it and just use the
	# instance name, in square brackets.
	$dest = '['.clean_utf8($m->instance).']';
    }
    else
    {
	# If the defaults aren't being used, show both class and instance.
	$dest = clean_utf8($m->context).'['.clean_utf8($m->instance).']';
    }

    $dest =~ s/[[:cntrl:]]//g;

    # Extract user/authentication information from the message.
    my $user = clean_utf8($m->pretty_sender);

    my $auth;

    # I'm assuming I'll never see echoes of outbound non-personal zephyrs,
    # so these must be personals. For outbound personals, set username as
    # the recipient with '->' prepended, set auth to '>' to indicate
    # outbound.
    if (lc($m->direction) eq 'out')
    {
	$user = "->".clean_utf8($m->recipient);
	$user =~ s/\@ATHENA\.MIT\.EDU//;
	$dest = '[personal]';
	$auth = '>';
    } else {
        $auth = (($m->auth =~ /YES/) ? '+' : '-');
    }

    my ($body, $hostSep) = format_body($m);

    my $zVT = "";
    my $cols = owl::getnumcols();
    if ($cols < $VT_Options{"narrowMode"})
    {
	#This formats the zephyr for smaller screens.

	$cols -= 3;
	if ($cols < 50)
	{
	    #	      1
	    #1234567890123456789
	    #_username_ HH:MM A
	    my $wDest = $cols - 19;
	    my $fmt = "%-10.10s %5s $auth %-".$wDest.".".$wDest."s\n %s";
	    $zVT = sprintf($fmt,
			   $user,
			   $time,
			   $dest,
			   $body);
	}
	else
	{
	    # Prepare the hostname.
	    my $hostStr = uc($m->host);
	    $hostStr =~ s/\.MIT\.EDU$// if $VT_Options{"stripMitEdu"};

	    my $rest  = $cols - 50;

	    my $wDest = 16 + (($rest <= 14) ? $rest : 14 );
	    $rest -= $wDest - 16;

	    my $wUser = 10 + (($rest <= 2) ? $rest : 2);
	    $rest -= $wUser - 10;

	    my $wHost = 14 + (($rest <= 10) ? $rest : 10);
	    $rest -= $wHost - 14;

	    $wDest += $rest;

	    my $fmt =  "%-".$wUser.".".$wUser."s %5s $auth %-".$wDest.".".$wDest."s %".$wHost."s\n %s";

	    $zVT = sprintf($fmt,
			   $user,
			   $time,
			   $dest,
			   $hostSep.' '.$hostStr,
			   $body);
	}
    }
    else
    {
	# This formats the zephyr for larger screens.
	$zVT = sprintf("%-10.10s %5s %-18.18s $auth%s",
		       $user,
		       $time,
		       $dest,
		       $body);
    }

    if (($m->is_personal || lc($m->direction) eq 'out'))
    {
	return boldify($zVT);
    }
    return $zVT;
}

################################################################################
# Functions to format AIM messages.
################################################################################
sub format_VT_AIM($)
{
    my $m = shift;

    # Extract time from message
    my $time = format_time($m);

    # Deal with login/logout messages
    if ($m->is_login())
    {
	return sprintf("\@b(%-10.10s) %s \@b(%s)",
		       "AIM LOGIN",
		       $time,
		       $m->sender);
    }

    if ($m->is_logout())
    {
	return sprintf("\@b(%-10.10s) %s \@b(%s)",
		       "AIM LOGOUT",
		       $time,
		       $m->sender);
    }

    # Extract destination from message
    my $dest = $m->recipient;

    # Extract user/authentication information from the message.
    my $user = $m->sender;

    my $dir = (lc($m->direction) eq 'out') ? '>' : '<';

    my ($body, $hostSep) = format_body($m);

    # Now build the message.
    my $zVT = "";
    if (owl::getnumcols() < $VT_Options{"narrowMode"})
    {
	$zVT = sprintf("From: %-16.16s To: %-16.16s %5s\n %s",
		       $user,
		       $dest,
		       $time,
		       $body);
    }
    else
    {
	$zVT = sprintf("%-10.10s %5s %-18.18s $dir%s",
		       $user,
		       $time,
		       $dest,
		       $body);
    }
    return boldify($zVT);
}

################################################################################
# Functions to format jabber messages.
################################################################################
sub format_VT_jabber($)
{
    my $m = shift;

    # Extract time from message
    my $time = format_time($m);

    # Deal with login/logout messages
    if ($m->is_login())
    {
      my $show = $m->{show};
      my $status = $m->{status};
      my $appendStr = "";
      $appendStr .= "$show" if ($show);
      $appendStr .= ", $status" if ($status);
      $appendStr = " ($appendStr)" if $appendStr;
      return sprintf("\@b(%-10.10s) %s %s",
		     "LOGIN",
		     $time,
		     boldify($m->sender.$appendStr));
    }

    if ($m->is_logout())
    {
      return sprintf("\@b(%-10.10s) %s %s",
		     "LOGOUT",
		     $time,
		     boldify($m->sender));
    }

    my $dir = (lc($m->direction) eq 'out') ? '>' : '<';

    # Extract destination from message
    my $dest = $m->recipient;
    my $hostStr = '';
    if (!$m->is_personal) {
      #MUC
      $dest =~ s/\@(conference.mit.edu)//;
      $hostStr = uc($1);
      $dest .= "[".$m->{subject}."]"if ($m->{subject});
      $dir = '*'
    }
    # Extract user information from the message.
    my $user = $m->sender;
    $user =~ s/\xE2\x99\xb3/1/g; # Deal with recycling symbol of raeburn's. Remove after UTF-8 is supported.

    my ($body, $hostSep) = format_body($m);

    # Now build the message.
    my $zVT = "";
    my $cols = owl::getnumcols();
    if ($cols < $VT_Options{"narrowMode"})
    {
        my $wHost = $cols - (3+6+16+5+16+1+5+1);
	$zVT = sprintf("From: %-16.16s To: %-16.16s %5s %".$wHost."s\n %s",
		       $user,
		       $dest,
		       $time,
		       ($hostStr ? $hostSep.' '.$hostStr : ''),
		       $body);
    }
    else
    {
	$zVT = sprintf("%-10.10s %5s %-18.18s $dir%s",
		       $user,
		       $time,
		       $dest,
		       $body);
    }
    if (($m->is_personal || lc($m->direction) eq 'out'))
    {
	return boldify($zVT);
    }
    return $zVT;
}

################################################################################
# Functions to format IRC messages.
################################################################################
sub format_VT_IRC($)
{
    my $m = shift;

    # Extract time from message
    my $time = format_time($m);

    # Deal with login/logout messages
    if ($m->is_login())
    {
      my $chan = $m->channel;
      return sprintf("\@b(%-10.10s) %s %s",
		     "JOIN",
		     $time,
		     boldify($m->sender . (defined($chan) ? " ($chan)" : "")));
    }

    if ($m->is_logout())
    {
        if(defined($m->channel)) {
            my $chan = $m->channel;
            return sprintf("\@b(%-10.10s) %s %s",
                           "PART",
                           $time,
                           boldify($m->sender . " ($chan)"));
        } else {
            my $reason = $m->{reason};
            return sprintf("\@b(%-10.10s) %s %s",
                           "QUIT",
                           $time,
                           boldify($m->sender . " ($reason)"));
        }
    }

    my $dir = (lc($m->direction) eq 'out') ? '>' : '<';

    # Extract destination from message
    my $dest = $m->recipient;
    my $hostStr = $m->server;

    # Extract user information from the message.
    my $user = $m->sender;

    my ($body, $hostSep) = format_body($m);

    # Now build the message.
    my $zVT = "";
    my $cols = owl::getnumcols();
    if ($cols < $VT_Options{"narrowMode"})
    {
        my $wHost = $cols - (3+6+16+5+16+1+5+1);
	$zVT = sprintf("From: %-16.16s To: %-16.16s %5s %".$wHost."s\n %s",
		       $user,
		       $dest,
		       $time,
		       ($hostStr ? $hostSep.' '.$hostStr : ''),
		       $body);
    }
    else
    {
	$zVT = sprintf("%-10.10s %5s %-18.18s $dir%s",
		       $user,
		       $time,
		       $dest,
		       $body);
    }
    if (($m->is_personal || lc($m->direction) eq 'out'))
    {
	return boldify($zVT);
    }
    return $zVT;
}

sub url_title {
    my $url = shift;
    my $mech = WWW::Mechanize->new;
    $mech->get($url);
    return clean_utf8($mech->title);
}

sub youtube_title {
    my $url = shift;
    my $title = url_title($url);
    $title =~ s/^YouTube - //;
    return $title;
}

sub tag_youtube {
    my $body = shift;
    $body =~ s{(http://(?:www[.])?youtube[.]com/watch\S+)}{"$1 [".youtube_title($1)."]"}ge;
    return $body;
}

################################################################################
# Functions to format Twitter messages.
################################################################################

sub format_twitter_links {
    my $body = shift;
    my @users;
    $body =~ s/@(\w+)/push @users, $1; "${1} [@{[scalar @users]}]"/ge;
    my $i = 1;
    for my $u (@users) {
        $body .= "\n[$i] http://twitter.com/$u";
        $i++;
    }
    return $body;
}

sub format_VT_twitter($)
{
    my $m = shift;

    # Extract time from message
    my $time = format_time($m);

    my $user = $m->sender;
    $user =~ s/\xE2\x99\xb3/1/g; 

    my ($body, $hostSep) = format_body($m);

    # Now build the message.
    my $zVT = "";
    my $cols = owl::getnumcols();
    if ($cols < $VT_Options{"narrowMode"})
    {
        my $wHost = $cols - (3+6+16+5+16+1+5+1);
	$zVT = sprintf("From: %-16.16s To: %-16.16s %5s \n%s",
		       $user,
		       "twitter",
		       $time,
		       $body);
    }
    else
    {
	$zVT = sprintf("%-10.10s %5s %-18.18s <%s",
		       $user,
		       $time,
		       "twitter",
		       $body);
    }
    if($m->is_private) {
        return boldify($zVT);
    } else {
        return $zVT;
    }
}

################################################################################
# Universal body formatter.
################################################################################
sub format_body
{
  my $m = shift;
  my $cols = owl::getnumcols();	# actual number of columns 
  my $width = $cols - 3;	# max usable width
  my $hwidth = ($cols < $VT_Options{"narrowMode"}) ? 2 : 38; # body header width / body indent
  my $bwidth = $width - $hwidth; # body width
  my $zsindent = ($cols < $VT_Options{"narrowMode"}) ? 1 : 18; # zsig indent width (zephyrs only)
  my $zsbwidth = $width - $zsindent; # zsig body width (zephyrs only)

  my $strlen = 0;
  my $body = "";
  my $hostAlone = 0;

  # Zephyrs only: This shows me if there are literal backspaces in the
  # zephyr body or zsig.
  my $hostSep = ($m->body =~ /\cH/ || ($m->zsig||"") =~ /\cH/) ? "!#" : "##";

  my $rawBody = $m->body;

  $rawBody =~ s/\r/^M/g;

  # Deal with literal backspaces by interpreting them or revealing them.
  if ($VT_Options{"showControl"}) {
    $rawBody =~ s/[\cH]/^H/g;
  } else {
    1 while $rawBody =~ s/[^\cH]\cH//g;
    $rawBody =~ s/[\cH]//g;
  }

  # This cleans up other peoples formatting. I can see what they meant, but it
  # doesn't muck with my display. 
  # Basically, double up the '@'s in these formatting messages such that they
  # no longer work. Also, fix backspace issues.
  $rawBody =~ s/\@font\(fixed\)$//; # GAIM is broken.
  $rawBody = zescape($rawBody);

  if($m->type eq 'zephyr' && $m->class eq 'MAIL') {
      $rawBody = decode('MIME-Header', $rawBody);
  } else {
      $rawBody = clean_utf8($rawBody);
  }

  if(lc $m->type eq 'twitter') {
      $rawBody = format_twitter_links($rawBody);
  }

  $rawBody = tag_youtube($rawBody);

  # This is a really dumb formatting test. If the message has any newlines 
  # followed by whitespace followed by non whitespace, I'll assume the sender
  # knows what they're doing and format the message as they desire.
  if ($rawBody =~ /\n[ \t]+\S.*\n/) {
    # Strip multiple and trailing newlines, then get an array of lines.
    $rawBody =~ s/\n+/\n/g;
    $rawBody =~ s/\n*$//g;
    my @lines = split (/\n/, $rawBody);

    # build the body, taking into account the desired indenting.
    my $line = shift @lines;
    $body .= " $line";
    $strlen = length($line);

    foreach my $l (@lines) {
      $body .= "\n";
      $body .= " " x ($hwidth - 1);
      $body .= " $l";
      $strlen = length($l);
      $line = $l;
    }

    my @count = split(/\@\@/, $line);
    if ($#count == -1) {
      $strlen -= length($line) / 2;
    } elsif ($#count) {
      $strlen -= $#count;
    }


  }
  # If the formatting does not pass the above test, I'm rewrapping the entire
  # message to my liking.
  else {
    if($m->type eq 'AIM') {
        my $wc = HTML::WikiConverter->new(dialect => 'Markdown');
        $rawBody = $wc->html2wiki($rawBody);
        $rawBody =~ s/\\(?=[`*_\\])//g;
        my %esc = (
            gt   => '>',
            lt   => '<',
            amp  => '&',
            quot => '"',
           );
        $rawBody =~ s/&(\w+);/$esc{$1}/eg;
    }
    $body = autoformat $rawBody, {left => $hwidth + 1,
                                  right => $hwidth + $bwidth - 2,
                                  all => 1,
                                  renumber => 0};
    $body = "" unless defined($body);
    $body =~ s/^\s+/ /;
    $body =~ s/\s+$//;
  }

  if ($m->is_zephyr) {
    # Now that the body is done, we deal with formatting the zsig, if desired.
    if ($VT_Options{"zsigs"} && $m->zsig ne "") {
      $hostAlone = 0;
      $body .= "\n";
      $body .= " " x $zsindent;
      $body .= "--";

      my $sig = $m->zsig;

      $sig =~ s/.\cH//g;
      # Kill leading whitespace
      $sig =~ s/^\s*//;

      $sig = zescape($sig);

      # Unlike zephyr bodies, I'm unwrapping zsigs no matter what.
      my @words = split (/\s+/, $sig);

      $strlen = 2;	    #takes into account the '--' we've put in.

      foreach my $word (@words) {
	$hostSep = '!#' if ($word =~ /[[:cntrl:]]/);
	if (!$VT_Options{'showControl'}) {
	  $word =~ s/[[:cntrl:]]//go;
	}
	if (($strlen + length($word) + 1) < $zsbwidth) {
	  $body .= " $word";
	  $strlen += 1 + length($word);
	} else {
	  $body .= "\n";
	  $body .= " " x $zsindent;
	  # The three extra spaces keep the zsig body lined up.
	  # Remember the '-- '?
	  $body .= "   $word";
	  $strlen = 3 + length($word);
	}
	# And again with the '@@' => '@' processing.
	my @count = split(/\@\@/, $word);
	if ($#count == -1) {
	  $strlen -= length($word) / 2;
	} elsif ($#count) {
	  $strlen -= $#count;
	}
      }
    }
  }
  if ($m->is_zephyr || ($m->type eq 'IRC') || ($m->type eq 'jabber' && !$m->is_personal)) {
    # Finally append the hostname. If it will fit on the last line of the
    # zephyr, that's great, if not it gets a line of its own. The hostname is
    # right justified. This only happens in the large screen formatting style.
    if ($cols >= $VT_Options{"narrowMode"}) {
      my $hostwidth = (!($VT_Options{"zsigs"} && $m->zsig ne "")
			 ? $bwidth 
			   : $zsbwidth);

      my $hostStr;
      if ($m->is_zephyr) {
	$hostStr = uc($m->host);
	$hostStr =~ s/\.MIT\.EDU$// if $VT_Options{"stripMitEdu"};
      } elsif ($m->type eq 'IRC') {
        $hostStr = uc $m->server;
      } else {
	$hostStr = uc($m->room);
	$hostStr =~ s/.*\@//;
      }

      $strlen = (length BarnOwl::ztext_stylestrip($1)) if $body =~ /\s+(\S.+)$/;
      if ($hostAlone || (($strlen + 4 + length($hostStr)) >= $hostwidth)) {
	$body .= "\n";
	$body .= sprintf("%".($width)."s",
			 " $hostSep $hostStr");
      } else {
	$body .= " " x ($hostwidth - $strlen - 4 - length($hostStr));
	$body .= " $hostSep $hostStr";
      }
    } else {
      $body .= " " x ($bwidth - $strlen);
    }
  } else {
    $body .= " " x ($bwidth - $strlen);
  }
  return ($body, $hostSep);
}

1;
