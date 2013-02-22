#! /usr/bin/perl -w
#
# listadmin - process messages held by Mailman for approval
# Written 2003 - 2007 by
# Kjetil Torgrim Homme <kjetilho+listadmin@ifi.uio.no>
#
# Thank you, Sam Watkins and Bernie Hoeneisen, for contributions and
# feedback.
#
# Released into public domain.

my $version = "2.40";
my $maintainer = "kjetilho+listadmin\@ifi.uio.no";

use HTML::TokeParser;
use LWP::UserAgent;
# use LWP::Debug qw(+trace);
use MIME::Base64;
use MIME::QuotedPrint;
use Data::Dumper;
use Term::ReadLine;
use Getopt::Long;
use Text::Reform;
use I18N::Langinfo qw(langinfo CODESET); # appeared in Perl 5.7.2
use Encode; # appeared in perl 5.7.1
use strict;
use English;

my $rc = $ENV{"HOME"}."/.listadmin.ini";

sub usage {
    my ($exit_val) = @_;
    print STDERR <<_end_;
Usage: $0 [-f CONFIGFILE] [-t MINUTES] [{-a|-r} FILE] [-l] [LISTNAME]
  -f CONFIGFILE    Read configuration from CONFIGFILE.
                   (default: $rc)
  -t MINUTES       Stop processing after MINUTES minutes.  Decimals are
                   allowed.
  --mail           Turn off "nomail" flag for the specified addresses
  --nomail         Turn on "nomail" flag for the specified addresses
  -a FILE          Add e-mail addresses in FILE to list
  -r FILE          Remove e-mail addresses in FILE to list
  --add-member ADDRESS
                   Add ADDRESS as member to list
  --remove-member ADDRESS
                   Remove ADDRESS from member list
  -l               List subscribers
  LISTNAME         Only process lists with name matching LISTNAME.

If options which modify members are used, LISTNAME must match exactly
one list.
_end_
    exit(defined $exit_val ? $exit_val : 64);
}

my ($opt_help, $opt_version, $opt_f, $opt_t, $opt_a, $opt_r,
    @opt_add_member, @opt_remove_member, $opt_l);
my $opt_mail = 1;

GetOptions("help|?" => \$opt_help,
	   "version|V" => \$opt_version,
           "f=s" => \$opt_f,
	   "t=i" => \$opt_t,
	   "mail!" => \$opt_mail,
	   "a=s" => \$opt_a,
	   "r=s" => \$opt_r,
	   "add-member=s" => \@opt_add_member,
	   "remove-member=s" => \@opt_remove_member,
	   "l" => \$opt_l)
	or usage();

usage(0) if $opt_help;
if ($opt_version) {
    print "listadmin version $version\n";
    exit(0);
}

$rc = $opt_f if $opt_f;
usage() if defined $opt_t && $opt_t !~ /\d/ && $opt_t !~ /^\d*(\.\d*)?$/;

push(@opt_add_member, read_address_file($opt_a, 1)) if defined $opt_a;
push(@opt_remove_member, read_address_file($opt_r, 1)) if defined $opt_r;

my $will_modify_membership = 0;
++$will_modify_membership if @opt_add_member;
++$will_modify_membership if @opt_remove_member;

usage() if $will_modify_membership > 1;
usage() if defined $opt_l && $will_modify_membership;

my $ua = new LWP::UserAgent("timeout" => 900, "env_proxy" => 1);
my $time_limit = time + 60 * ($opt_t || 24*60);
my $term;
my $term_encoding = langinfo(CODESET());

# the C and POSIX locale in Solaris uses the charset "646", but Perl
# doesn't support it.
$term_encoding = "ascii" if $term_encoding eq "646";
binmode STDOUT, ":encoding($term_encoding)";
# Turn on autoflush on STDOUT
$| = 1;

my $config = read_config ($rc);
unless ($config) {
    exit (0) unless prompt_for_config ($rc);
    $config = read_config ($rc);
}

my @lists = ();
if (@ARGV) {
    if (defined $config->{$ARGV[0]}) {
	push @lists, $ARGV[0];
    } else {
	@lists = sort config_order grep { /$ARGV[0]/o } keys %{$config}
    }
    if (@lists == 0) {
	print STDERR "$ARGV[0]: no matching list\n";
	usage();
    }
} else {
    @lists = sort config_order keys %{$config}
}

if (@lists > 1 && ($will_modify_membership || defined $opt_l)) {
    print STDERR "Too many matching lists\n";
    print Dumper(\@lists);
    usage();
}

my $list = $lists[0];

my $subscribe_result;
if (@opt_add_member) {
    $subscribe_result = add_subscribers($list, $config->{$list}, $opt_mail,
					@opt_add_member);
}
if (@opt_remove_member) {
    $subscribe_result = remove_subscribers($list, $config->{$list},
					   @opt_remove_member);
}
if (defined $subscribe_result) {
    for my $addr (keys %{$subscribe_result}) {
	print STDERR "$addr: $subscribe_result->{$addr}\n";
    }
    if (%{$subscribe_result}) {
	exit(1);
    } else {
	print "Ok\n";
	exit(0);
    }
}
if (defined $opt_l) {
    my @subscribers = list_subscribers($list, $config->{$list});
    print join("\n", @subscribers, "");
    exit(@subscribers == 0);
}

my ($num, $count, $from, $subject, $reason, $spamscore);


for (@lists) {
    $list = $_;
    my $user = $config->{$list}{"user"};
    my $pw = $config->{$list}{"password"} || "";

    if (time > $time_limit) {
	print "Time's up, skipping the remaining lists\n";
	last;
    }

    my $info = {};
    my $tries = 0;
    print "fetching data for $list ... ";
    do {
	if (-t && ($pw eq "" || $info->{'autherror'})) {
	    print "\n" unless $tries++;
	    $pw = prompt_password("Enter password" .
				  ($user ? " for $user: ": ": "));
	    next if $pw eq "";
	}
	$info = get_list($list, $config->{$list}, $pw);
	if ($info->{'autherror'}) {
	    print "\n" unless $tries++;
	    print STDERR "ERROR: Username or password for $list incorrect\n";
	}
    } while (-t && $info->{'autherror'} && $tries < 9);

    if ($info->{'servererror'}) {
	print "\n";
	printf STDERR ("ERROR: fetching %s\n", $info->{'url'});
	printf STDERR ("ERROR: %s -- skipping list\n",
		       $info->{'servererror'});
	next;
    } elsif ($info->{'autherror'}) {
	print "giving up, proceeding to next list\n";
	next;
    } elsif (! %{$info}) {
	print "nothing in queue\n";
	next;
    } else {
	print "\n";
    }
    $config->{$list}{"password"} = $pw;

    my %change = ();

    process_subscriptions ($info, $config->{$list}, \%change);
    $num = undef;
 restart_approval:
    approve_messages ($info, $config->{$list}, \%change);

    if ($config->{$list}->{"confirm"}) {
	if (scalar %change) {
	redo_confirm:
	    my $c = prompt ("Submit changes? [yes] ");
	    if ($c =~ /^\s*(\?+|h|hj?elp)\s*$/i) {
		print <<_END_;
Nothing will be done to the messages in the administrative queue
unless you answer this question affirmatively.  If you answer "no",
your changes will be discarded and listadmin will proceed to the
next mailing list.  Type "undo" to go back to the current list.
_END_
		goto redo_confirm;
	    }
	    if ($c =~ /^\s*(no?|nei|skip)\s*$/i) {
		print "skipping ...\n";
		next;
	    } elsif ($c =~ /^\d+$/) {
		$num = $c - 1;
		goto restart_approval;
	    } elsif ($c =~ /^u(ndo)?/) {
		--$num;
		goto restart_approval;
	    } elsif ($c !~ /^\s*(|ja?|y|yes)\s*$/i) {
		goto redo_confirm;
	    }
	}
    }
    print "\n";

    commit_changes ($list, $user, $pw, $config->{$list}{"adminurl"},
		    \%change, $info, $config->{$list}{"logfile"});
}

sub process_subscriptions {
    my ($info, $config, $change) = @_;
    my %subscribers = ();
    my $num = 0;
    for my $req (keys %{$info}) {
	if (exists $info->{$req}->{"subscription"}) {
	    $subscribers{$req} = $info->{$req}->{"subscription"};
	    delete $info->{$req};
	}
    }
    my $count = keys (%subscribers);
    my $def = $config->{"subdefault"};
    my $prompt = 'Accept/Discard/Reject/Skip/Quit';
    $prompt .= " [" . uc($def) . "]" if $def;
    $prompt .= " ? ";

 subscr_loop:
    for my $id (sort keys %subscribers) {
	last if time > $time_limit;
	++$num;
	print "\n[$num/$count] ========== $list ==========\n";
	print "From:    $subscribers{$id}\n";
	print "         subscription request\n";
	my $ans;
	while (1) {
	    $ans = $config->{"subaction"};
	    $ans ||= prompt ($prompt);
	    $ans = "q" unless defined $ans;
	    $ans =~ s/\s+//g;
	    $ans = $def if $ans eq "";
	    $ans = lc ($ans);
	    if ($ans eq "q") {
		last subscr_loop;
	    } elsif ($ans eq "s") {
		delete $change->{$id};
		next subscr_loop;
	    } elsif ($ans eq "a") {
		$change->{$id} = [ "sa" ];
		last;
	    } elsif ($ans eq "d") {
		$change->{$id} = [ "sd" ]; 
		last; 
	    } elsif ($ans eq "r") {
		my $r = prompt ("Why do you reject? [optional] ");
		unless (defined $r) {

		}
		$change->{$id} = [ "sr", $r ];
		last;
	    } else {
		print STDERR <<"_end_";
Choose one of the following actions by typing the corresponding letter
and pressing Return.

  a  Accept    -- allow the user to join the mailing list
  r  Reject    -- notify sender that the request was turned down
  d  Discard   -- silently discard the request
  s  Skip      -- do not decide now, leave it for later
  q  Quit      -- go on to approving messages

_end_
            }
	}
    }
}

sub approve_messages {
    my ($info, $config, $change) = @_;

    my $listdef = $config->{"default"};
    my $spamlevel = $config->{"spamlevel"};
    my $ns_from = $config->{"not_spam_if_from"};
    my $ns_subj = $config->{"not_spam_if_subject"};
    my $dis_from = $config->{"discard_if_from"};
    my $dis_subj = $config->{"discard_if_subject"};
    my $dis_reas = $config->{"discard_if_reason"};

    $count = keys (%{$info}) - 1;	# subtract 1 for globals
    my $search_pattern = "";
    my $dont_skip_forward = 0;
    if (!defined ($num)) {
	$num = 0;
    } else {
	$dont_skip_forward = 1;
    }
    my $tmpl_header = << '_end_';

<<<<<<<<<<<<<<<<<<<< <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
_end_
    my $tmpl_message = << '_end_';
From:     <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
<<<<<<<<  [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
Reason:   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Spam? <<<
_end_

    my $prompt = 'Approve/Reject/Discard/Skip/view Body/Full/jump #/Undo/Help/Quit';
    my @num_to_id = grep { ! /^global$/ } sort keys %{$info};
    my @undo_list = ();
 msgloop:
    while ($num < $count) {
	last if time > $time_limit;
	my $id = $num_to_id[$num++];
	$from = $info->{$id}{"from"};
	$subject = $info->{$id}{"subject"} || "";
	$reason = $info->{$id}{"reason"};
	$spamscore = $info->{$id}{"spamscore"};
	{
	    # Get rid of warning from Encode:
	    # "\x{516b}" does not map to iso-8859-1 at listadmin.pl line 261.
	    # when run in non UTF-8 environment.
	redraw:
	    local $SIG{__WARN__} = sub {};
	    print form({filler => {left => "=", right => "="}},
		       $tmpl_header,
		       "[$num/$count] =", "$list =");
	    print form({interleave => 1},
		       $tmpl_message,
		       $from,
		       "Subject:", $subject, $reason, $spamscore);
	}
	while (1) {
	    my $ans;
	    my $match = "";
	    if ($spamlevel && $spamscore >= $spamlevel) {
		$match = "spam"; $ans = "d";
	    }
	    $ans ||= $config->{"action"};
	    $match = "From" if got_match ($from, $dis_from);
	    $match = "Subject"
		    if $dis_subj && got_match ($subject, $dis_subj);
	    $match = "reason"
		    if $dis_reas && got_match ($reason, $dis_reas);
	    $ans ||= "d" if $match;
	    $ans = undef if (($ns_subj && $subject =~ $ns_subj) ||
			     ($ns_from && $from =~ $ns_from) ||
			     $dont_skip_forward);

	    if ($ans && $match) {
		if ($match eq "spam") {
		    print "Automatically discarded as spam.\n";
		} else {
		    print "Automatically discarded due to matching $match\n";
		}
		$ans = "d";
	    }
	    my $def = $listdef;
	    $def = $change->{$id}->[0]
		    if defined $change->{$id};
	    my $pr = $prompt;
	    $pr .= " [" . uc($def) . "]" if $def;
	    $pr .= " ? ";
	    $ans ||= prompt ($pr);
	    $ans = "q" unless defined $ans;
	    $ans =~ s/^\s+//;
	    $ans =~ s/\s+$//;
	    $ans = $def if $ans eq "" && defined $def;
	    $ans = lc $ans;
	    if ($ans eq "q") {
		last msgloop;
	    } elsif ($ans eq "s") {
		# Undo will be a no-op, except it will go back to this message.
		push(@undo_list, [$num]);
		delete $change->{$id};
		$dont_skip_forward = 0;
		next msgloop;
	    } elsif ($ans =~ /^\d+$/ && $ans > 0 && $ans <= $count) {
		$num = $ans - 1;
		$dont_skip_forward = 1;
		next msgloop;
	    } elsif ($ans eq "a" || $ans eq "d") {
		# If it is automatically discarded, add it to existing list
		push(@undo_list, []) unless $match && @undo_list;
		push(@{$undo_list[$#undo_list]}, $num);
		$change->{$id} = [ $ans ];
		$dont_skip_forward = 0;
		last;
	    } elsif ($ans eq "u") {
		unless (@undo_list) {
		    print "Nothing to undo.\n";
		    next;
		}
		my @trans_list = @{pop(@undo_list)};
		for my $m (@trans_list) {
		    delete $change->{$num_to_id[$m - 1]};
		}
		$num = $trans_list[0] - 1;
		$dont_skip_forward = 1;
		next msgloop;
	    } elsif ($ans =~ /^list(\s+|$)/) {
		my @list = list_subscribers($list, $config);
		my $member_count = scalar @list;
		if ($POSTMATCH ne "") {
		    @list = grep { /$POSTMATCH/ } @list;
		    printf("Found %d matching addresses:\n  ", scalar @list);
		} else {
		    print "Mailing list members:\n  ";
		}
		print join("\n  ", @list);
		print "\n$member_count members in total\n";
	    } elsif ($ans =~ /^(add|nomail)(\s+|$)/) {
		my $mail = $1 eq "add";
		my $addr = $POSTMATCH || $from;
		my $res = add_subscribers($list, $config, $mail, $addr);
		for my $addr (keys %{$res}) {
		    print "$addr: $res->{$addr}\n";
		}
		print "done\n";
	    } elsif ($ans =~ /^rem(\s+|$)/) {
		my $address = $POSTMATCH;
		my $c = prompt ("Remove subscriber? (there is no undo!) [no] ");
		if ($c =~ /^\s*(ja?|y|yes)\s*$/i) {
		    print "removing...\n";
		    my $res = remove_subscribers($list, $config, $address);
		    for my $addr (keys %{$res}) {
			print "$addr: $res->{$addr}\n";
		    }
		    print "done\n";
		} else {
		    print "aborted\n";
		    next;
		}
	    } elsif ($ans =~ m,([/?])(.*),) {
		my $i = $num - 1;
		my $direction = 1;
		my $fencepost = $count - 1;
		if ($1 eq "?") {
		    $direction = -1;
		    $fencepost = 1;
		}
		# If no pattern is specified, reuse previous pattern.
		$search_pattern = $2 unless $2 eq "";
		if ($search_pattern eq "") {
		    print "No search pattern specified.  Try 'help'\n";
		    next;
		}
		while ($i != $fencepost) {
		    $i += $direction;
		    my $id = $num_to_id[$i];
		    my $search_from = $info->{$id}{"from"};
		    my $search_subject = $info->{$id}{"subject"} || "";
		    if ($search_from =~ /$search_pattern/i ||
			$search_subject =~ /$search_pattern/i) {
			$num = $i;
			$dont_skip_forward = 1;
			next msgloop;
		    }
		}
		print "Pattern not found\n"
	    } elsif ($ans eq "r") {
	    redo_reject:
		my $def_reason = $info->{$id}{"rejreason"};
		$def_reason = $change->{$id}->[1]
			if defined $change->{$id} && $change->{$id}->[0] eq "r";
		my $r = prompt ("Why do you reject? ", $def_reason);
		if ($r =~ /^\s*$/) {
		    print "aborted\n";
		    next;
		} elsif ($r =~ /^\s*(\?+|h|help)\s*$/i) {
		    print "The reason entered will be included in the e-mail ".
			    "sent to the submitter.\n";
		    goto redo_reject;
		}

		push(@undo_list, [ $num ]);
		$change->{$id} = [ "r", $r ];
		$dont_skip_forward = 0;
		last;
	    } elsif ($ans eq "f") {
		# Since the raw bytes aren't really Unicode, we set
		# the replacement sequence to be "<?>" unconditionally.
		print degrade_charset($info->{$id}{"headers"} . "\n\n" .
				      $info->{$id}{"body"}, "questionmark");
	    } elsif ($ans eq "b") {
		my $head = $info->{$id}{"headers"};
		my $text = $info->{$id}{"body"};
		my $mime_headers = "";
		if ($head =~ m,content-type:\s*text/,i) {
		    $mime_headers = $head;
		} elsif ($head =~ m,content-type:\s*multipart/,i) {
		    # This is quick and dirty, we look at the first
		    # MIME headers in the body instead.  We can't do
		    # proper MIME parsing since the message is
		    # truncated by Mailman.
		    $mime_headers = $text;
		}
		if ($mime_headers =~ /content-transfer-encoding:\s+(\S+)/i) {
		    my $cte = $1;
		    if ($cte =~ /quoted-printable/i) {
			$text = MIME::QuotedPrint::decode($text);
		    } elsif ($cte =~ /base64/i) {
			# Don't bother with truncated lines.
			$text =~ s!([A-Za-z0-9/+=]{72,76})!MIME::Base64::decode_base64($1)!ge;
		    }
		}
		if ($mime_headers =~ /charset=(\S+)/i) {
		    my $charset = $1;
		    $charset =~ s/;$//;
		    $charset =~ s/^"(.*)"$/$1/;
		    $charset = guess_charset($charset, $text);
		    eval { $text = Encode::decode($charset, $text) };
		}

		$text = degrade_charset($text, $config->{unprintable});
		my @lines = split (/\n/, $text, 21);
		pop @lines;
		# local $SIG{__WARN__} = sub {}; # see comment elsewhere
		print join ("\n", @lines), "\n";
	    } elsif ($ans eq "t") {
		print $info->{$id}{"date"}, "\n";
	    } elsif ($ans eq "url") {
		print mailman_url($list, $config->{adminurl}), "\n";
	    } elsif ($ans eq ".") {
		goto redraw;
	    } elsif ($ans eq "") {
		# nothing.
	    } else {
		print <<"end";
Choose one of the following actions by typing the corresponding letter
and pressing Return.

  a  Approve     -- the message will be sent to all members of the list
  r  Reject      -- notify sender that the message was rejected
  d  Discard     -- throw message away, don't notify sender
  s  Skip        -- don't decide now, leave it for later
  b  view Body   -- display the first 20 lines of the message
  f  view Full   -- display the complete message, including headers
  t  view Time   -- display the date the message was sent
  #  jump        -- jump backward or forward to message number #
  u  Undo        -- undo last approve or discard
  /pattern       -- search for next message with matching From or Subject
  ?pattern       -- search for previous message with matching From or Subject
  .              -- redisplay entry
  add [address]  -- add subscription for address (defaults to From)
  nomail [address] -- add nomail subscription for address (defaults to From)
  list [pattern] -- list mailing list members matching optional pattern
  rem address    -- remove list member
  q  Quit        -- go on to the next list

end
		print <<"end" if $listdef;
The default action for this list when you only press Return is '$listdef'

end
            }
	}
    }
}

sub url_quote_parameter {
    my $param = shift;
    $param =~ s/(\W)/sprintf ("%%%02x", ord ($1))/ge;
    $param;
}

sub mailman_params {
    my ($user, $pw) = @_;
    my %params;
    $params{"username"} = $user if defined $user;
    $params{"adminpw"} = $pw if defined $pw;
    return \%params;
}

sub uio_adminurl {
    my ($domain) = @_;
    return 'https://{domain}/mailman/{domain}/admindb/{list}'
	    if ($domain eq 'lister.ping.uio.no');
    return 'http://{domain}/mailman/admindb/{list}@{domain}'
	    if ($domain eq "lister.uio.no");
    return 'http://{subdomain}-lists.uio.no/mailman/admindb/{list}@{domain}'
	    if ($domain =~ /^(\w+\.)?uio\.no$/);
    return 'http://lists.{domain}/mailman/admindb/{list}@{domain}'
	    if ($domain eq "simula.no");
    undef;
}

sub mailman_url {
    my ($list, $pattern, $params, $action) = @_;

    my ($lp, $domain) = split ('@', $list);

    $pattern ||= uio_adminurl ($domain);
    $pattern ||= 'http://{domain}/mailman/admindb/{list}';

    my $url = $pattern;
    my $subdom = $domain;
    $subdom = $PREMATCH if $subdom =~ /\./;
    $url =~ s/\{list\}/$lp/g;
    $url =~ s/\{domain\}/$domain/g;
    $url =~ s/\{subdomain\}/$subdom/g;
    if ($action) {
	$url =~ s,/admindb/,/admin/,;
	$url .= "/$action";
    }
    $url .= "?$params" if $params;
    return $url;
}

# Returns a ref to a hash with all the information about pending messages
sub get_list {
    my ($list, $config, $pw) = @_;

    my $starttime = time;
    my $mmver;
    my ($page, $page_appr, $resp_appr);
    my $url = mailman_url($list, $config->{"adminurl"});
    my $resp = $ua->post($url, mailman_params($config->{"user"}, $pw));
    unless ($resp->is_success) {
	return {'servererror' => $resp->status_line, 'url' => $url};
    }
    $page = $resp->content;

    my $dumpdir = $config->{"dumpdir"};
    my $dumpfile;
    if ($dumpdir && $page) {
	$dumpfile = "$dumpdir/dump-$list.html";
	if (open (DUMP, ">$dumpfile")) {
	    print DUMP $page;
	    close (DUMP);
	}
    }

    if ($page eq "") {
	if (time - $starttime > 60) {
	    return {servererror => "Mailman server timed out?", url => $url};
	} else {
	    return {servererror => "Empty page", url => $url};
	}
    } elsif ($page =~ get_trans_re("no_such_list")) {
	return {servererror => "No such list", url => $url}
    }

    my $parse = HTML::TokeParser->new(\$page) || die;
    $parse->get_tag ("title") || die;
    my $title = $parse->get_trimmed_text ("/title") || die;

    if ($title =~ get_trans_re("authentication")) {
	return {'autherror' => 1};
    }

    if ($page !~ get_trans_re("pending_req")) {
	my $msg = "unexpected contents";
	# Use rand() to protect a little against tmpfile races
	$dumpfile ||= "/tmp/dump-" . rand() . "-$list.html";
	if (open(DUMP, ">$dumpfile")) {
	    chmod(0600, $dumpfile);
	    print DUMP $page;
	    close(DUMP);
	    $msg .= ", please send $dumpfile to $maintainer";
	}
	return {servererror => $msg, url => $url};
    }

    my @mailman_mentions = grep {/Mailman/} split (/\n/, $page);
    for my $mention (reverse @mailman_mentions) {
	if ($mention =~ /\bv(ersion)?\s(\d+\.\d+)/) {
	    $mmver = $2;
	    last;
	}
    }
    unless ($mmver) {
	die "Can not find version information, please mail maintainer.";
    }

    if ($mmver ge "2.1") {
	# Mailman does not look for "details" in parameters, so it
	# must be part of the query string.
	$url = mailman_url($list, $config->{"adminurl"}, "details=all");
	$resp = $ua->post($url, mailman_params($config->{"user"}, $pw));
	unless ($resp->is_success) {
	    return {'servererror' => $resp->status_line, 'url' => $url};
	}
	$page_appr = $resp->content;
	if (defined $dumpdir &&
	    open (DUMP, ">$dumpdir/dump-details-$list.html")) {
	    print DUMP $page_appr;
	    close (DUMP);
	}
    }

    my $data;
    if ($mmver ge "2.1") {
	my $parse_appr = HTML::TokeParser->new(\$page_appr) || die;
	$data = parse_pages_mm_2_1($mmver, $config, $parse, $parse_appr);
    } else {
	$data = parse_pages_mm_old($mmver, $config, $parse);
    }
    set_param_values($mmver, $data) if %{$data};
    return $data;
}

sub parse_pages_mm_old {
    my ($mmver, $config, $parse) = @_;

    my %data = ();
    my $token;
    $parse->get_tag ("hr");
    $parse->get_tag ("h2") || return \%data;
    my $headline = $parse->get_trimmed_text ("/h2") || die;
    if ($headline =~ get_trans_re("headline_subscr")) {
	parse_subscriptions ($mmver, $config, $parse, \%data);
	$token = $parse->get_token;
	if (lc ($token->[1]) eq "input") {
	    return (\%data);
	} else {
	    $parse->get_tag ("h2") || die;
	    $headline = $parse->get_trimmed_text ("/h2") || die;
	}
    }
    if ($headline =~ get_trans_re("held_for_approval")) {
	parse_approvals ($mmver, $config, $parse, \%data);
    } else {
	$parse->get_tag ("hr") || die;
	$token = $parse->get_token;
	if ($token->[0] eq "S" && lc ($token->[1]) eq "center") {
	    parse_approvals ($mmver, $config, $parse, \%data);
	}
    }
    return (\%data);
}

sub parse_pages_mm_2_1 {
    my ($mmver, $config, $parse_subs, $parse_appr) = @_;

    my %data = ();
    my $headline;

    # some (newer?) servers show only 1 <hr> tag when there is no subscriptions
    # Try resolve first seen <hr> as subscription, and fall back to approvals
    $parse_subs->get_tag ("hr");
    if ($parse_subs->get_tag ("h2")) {
	my $title = $parse_subs->get_trimmed_text ("/h2") || die;
	if ($title =~ get_trans_re("subscriptions")) {
	    parse_subscriptions ($mmver, $config, $parse_subs, \%data);

	    $parse_appr->get_tag ("hr");
	    if ($parse_appr->get_tag ("h2")) {
		parse_approvals ($mmver, $config, $parse_appr, \%data);
	    }
	} else {
	    parse_approvals ($mmver, $config, $parse_appr, \%data);
	}
    } else {
	$parse_appr->get_tag ("hr");
	if ($parse_appr->get_tag ("h2")) {
	    parse_approvals ($mmver, $config, $parse_appr, \%data);
	}
    }
    return (\%data);
}

sub parse_subscriptions {
    my ($mmver, $config, $parse, $data) = @_;
    my $token;

    $parse->get_tag ("table") || die;
    $parse->get_tag ("tr") || die;
    $parse->get_tag ("tr") || die;
    do {
	parse_subscription ($mmver, $config, $parse, $data);
	do {
	    $token = $parse->get_token;
	} until ($token->[0] eq "S");
    } while (lc ($token->[1]) eq "tr");
}

sub parse_subscription {
    my ($mmver, $config, $parse, $data) = @_;

    $parse->get_tag ("td") || die;
    my $address = $parse->get_trimmed_text ("/td") || die;
    my $tag = $parse->get_tag ("input") || die;
    my $id = $tag->[1]{"name"};
    $parse->get_tag ("/table") || die;
    $parse->get_tag ("/tr") || die;
    $data->{$id} = { "subscription" => $address };
}

sub parse_approvals {
    my ($mmver, $config, $parse, $data) = @_;
    my $token;

    do {
	$parse->get_tag ("table") || die;
	parse_approval ($mmver, $config, $parse, $data);
	$parse->get_tag ("/table");
	$parse->get_tag ("hr");
	$token = $parse->get_token;
	$token = $parse->get_token
		if ($token->[0] eq "S" && lc ($token->[1]) eq "center");
    } until ($token->[0] eq "S" && lc ($token->[1]) eq "input");
}

sub get_trans_re {
    my ($key) = @_;

    # Handle translations -- poorly...
    #
    # For now, we look for strings in all languages at the same time
    # since they don't seem to overlap.  This might have to change
    # later.
    #
    # Please send additions if you have them.

    # Below strings are found in source of Mailman 2.1.10 and "washed":
    # * high-bit chars and html ligatures in latin charsets replaced with .*
    #   (\S would be better but for some reason the code chokes on that)
    # * non-latin charsets included as-is and (if not already) as utf-8
    # * trailing punctuation stripped (to allow small changes to locales)
    my %translations =
	     # grep -ri -- '<title>' templates/*/admlogin.html
	    ("authentication" =>
	     {
	      "ar" => "التحقق من الشخصية لـ .* للقائمة",
	      "ca" => "Authentication",
	      "cs" => "p.*ihl.*en.*",
	      "da" => "Login",
	      # include old string (possibly bogusly grabbed from PO file)
	      "de" => "Anmeldung|Authentifikation",
	      "en" => "Authentication",
	      "es" => "Autentificaci.*n",
	      "et" => "autoriseerimine",
	      "eu" => "Zerrendako .* Identifikatzen",
	      "fi" => "Authentication",
	      "fr" => "Authentification",
	      "gl" => "Autenticaci.*n",
	      "he" => "האימות של",
	      "hr" => "Autentikacija",
	      "hu" => "Azonos.*t.*s",
	      "ia" => "Authentication",
	      "it" => "Autenticazione",
	      # | recode EUC-JP..utf8
	      "ja" => "ǧ��|認証",
	      # | recode EUC-KR..utf8
	      "ko" => "������ ����|관리자 인증",
	      "lt" => "prisijungimas",
	      "nl" => "inloggen",
	      "no" => "Innlogging",
	      "pl" => "%(listname)s",
	      "pt" => "Authentication",
	      "pt_BR" => "Autentica.*o",
	      "ro" => "Autentificare",
	      # | recode koi8-r..utf8
	      "ru" => "��������������|Аутентификация",
	      "sk" => "prihlásenie",
	      "sl" => "Avtentikacija",
	      "sr" => "Authentication",
	      "sv" => "Inloggning",
	      "tr" => "Giri.*i",
	      "uk" => "Автентифікація",
	      "vi" => "Xác th.*c",
	      "zh_CN" => "Authentication",
	      "zh_TW" => "論壇 壇主驗證",
	     },
	     # grep -r -A 1 'msgid "Subscription Requests"' messages/*
	     "subscriptions" =>
	     {
	      "C" => "Subscription Requests",
	      "ar" => "طلبات التسجيل",
	      "ca" => "Petici.* de Subscripci.*",
	      "cs" => "Po.*adavky na p.*ihl.*en",
	      "da" => "Anmoder om medlemskab",
	      "de" => "Abonnement-Anfragen",
	      "es" => "Peticiones de suscripci.*n",
	      "et" => "Liitumisssoovid",
	      "eu" => "Harpidetza Eskakizunak",
	      "fi" => "Liittymispyynt.*j.*",
	      "fr" => "Requ.*tes d'abonnement",
	      "gl" => "Solicitudes de subscrici.*n",
	      "he" => "בקשות מנוי",
	      "hr" => "Zahtjevi za Pretplatom",
	      "hu" => "Feliratkozási k.*relmek",
	      "ia" => "Requestas de abonamento",
	      "it" => "Richieste di iscrizione",
	      # | recode EUC-JP..utf8
	      "ja" => "������|入会申請",
	      # | recode EUC-KR..utf8
	      "ko" => "���� ��|가입 결과",
	      "lt" => "Uþsisakymo Pra.*ymas",
	      "nl" => "Aanmeldingsverzoeken",
	      "no" => "S.*knader om medlemskap",
	      "pl" => "Pro.*by o zapisanie",
	      "pt" => "Pedidos de inscri.*o",
	      "pt_BR" => "Requisi.*es de Inscri.*o",
	      "ro" => "Cereri de abonare",
	      # | recode koi8-r..utf8
	      "ru" => "������� �� �������|Запросы на подписку",
	      "sk" => ".*iadosti o prihl.*senie",
	      "sl" => "Zahteve za prijavo",
	      "sr" => "Захтјеви за упис",
	      "sv" => "Ans.*kningar om medlemskap",
	      "tr" => "Listeye .*yelik .*stekleri",
	      "uk" => "Запити на підписку",
	      "vi" => "Y.*u c.*u .*ng k.*",
	      "zh_CN" => "订阅请求",
	      "zh_TW" => "訂閱申請",
	     },
	     # grep -r -A 1 'msgid "Successfully \(subscribed\|Unsubscribed\|Removed\):"' messages/*
	     "subscr_success" =>
	     {
	      # include old (mistyped, or are these case-insensitive?) uppercase
	      "C" => "Successfully (([uU]n)?subscribed|Removed)",
	      "ar" => "تم اشتراكه بنجاح|خطأ في تسجيل الاشتراك|تمت إزالته بنجاح",
	      "ca" => "Subscrit satisfact.*riament|Subscripci.* Cancel.*lada Satisfact.*riament|Eliminat satisfact.*riament",
	      "cs" => ".*sp.*n.* p.*ihl.*eni|.*sp.*n.* odhl.*eni|.*sp.*n.* odstran.*ni",
	      "da" => "Tilmelding er sket|Framelding udf.*rt|Framelding udf.*rt",
	      "de" => "Erfolgreich (eingetragen|beendete Abonnements|entfernt)",
	      "es" => "(Subscritos|Ha anulado su suscripci.*n|Ha sido borrado) satisfactoriamente",
	      "et" => "Lisati aadressid|Tellimus l.*petati|Edukalt eemaldatud",
	      "eu" => "Behar bezala harpidetuta|Behar Bezala Ezabatuta|Arrakastaz ezabatua",
	      "fi" => "Onnistuneesti liitetty|Erotettu onnistuneesti|Poistettu onnistuneesti",
	      "fr" => "Abonnement r.*ussi|R.*siliation r.*ussie|Abonnement r.*sili.* avec succ.*s",
	      "gl" => "Subscribiuse con éxito|Anulou a súa subscrición satisfactoriamente|Eliminouse satisfactoriamente",
	      "he" => "נרשם בהצלחה|מנוי בוטל בהצלחה|הוסר בהצלחה",
	      "hr" => "Uspje.*no (pretpla.*eni|Odjavljeni|Maknut)",
	      "hu" => "Sikeresen (fel.*rva|t.*r.*lve|t.*r.*lve)",
	      "ia" => "(Abonate|Disabonate|Removite) con successo",
	      "it" => "(Iscritti|Cancellati|Rimosso) con successo",
	      # | recode EUC-JP..utf8
	      "ja" => "(��|��|��)���³����λ|(入|退|退)会手続き完了",
	      # | recode EUC-KR..utf8
	      "ko" => "���������� (���Ե�|Ż���|���ŵ�) ����|성공적으로 (가입된|탈퇴된|제거된) 명단",
	      "lt" => "S.*kmingai (u.*sisak.*|atsisak.*|pa.*alinti)",
	      "nl" => "Met succes (aangemeld|afgemeld|verwijderd)",
	      "no" => "(P.*melding|Utmelding) utf.*rt",
	      "pl" => "Pomy.*lnie (zapisano|wypisano|usuni.*to)",
	      "pt" => "(Inscrito|Inscri.*o anulada|Removido) co?m sucesso",
	      "pt_BR" => "(Inscrito|Descadastrado|Removido) com [sS]ucesso",
	      "ro" => "Au fost (abona.*i|dezabona.*i) cu succes",
	      # | recode koi8-r..utf8
	      "ru" => "������� (���������|������� �������� ����|������)|Успешно (подписаны|удалена подписка для|удалены)",
	      "sk" => "Úspe.*ne (prihlásení|odhlásení|zmazaní)",
	      "sl" => "Uspe.*no (prijavljeni|odjavljen|odstranjeni)",
	      "sr" => "Успјешно (уписани|исписани|уклоњени)",
	      "sv" => "(Anm.*lan|Avanmlan) gjord",
	      "tr" => "Ba.*ar.*yla (.*ye yap.*ld.*|.*yelikten .*kar.*ld.*|Silindi)",
	      "uk" => "Успішно (підписано|видалено підписку|видалено)",
	      "vi" => "Đã đăng ký được|Đã bỏ đăng ký được|Đã gỡ bỏ được",
	      "zh_CN" => "成功订阅|成功取消订阅|成功删除",
	      "zh_TW" => "訂閱成功|退訂成功|成功除名",
	     },
	     # grep -r -A 1 'msgid "Error \(subscribing\|Unsubscribing\):"' messages/*
	     "subscr_error" =>
	     {
	      # include old (mistyped, or are these case-insensitive?) uppercase
	      "C" => "Error ([uU]n)?subscribing",
	      "ar" => "خطأ في (الاشتراك|إلغاء الاشتراك)",
	      "ca" => "Error (subscrivint|cancel.*lant la subscripci.*)",
	      "cs" => "Chyba p.*i (p.*ihla.*ov.*n.*|odhla.*ov.*n.*)",
	      "da" => "Fejl under (tilmelding|framelding)",
	      "de" => "Fehler beim (Abonnieren|Beenden des Abonnement)",
	      "es" => "Error dando de (alta|baja) la suscripci.*n",
	      "et" => "Viga aadresside lisamisel|Viga aadressi kustutamisel",
	      "eu" => "Errorea harpidetzan|Zerrenda uztean errorea",
	      "fi" => "Virhe (liitt.*ess.*|eroamisessa)",
	      "fr" => "Erreur lors de (l'abonnement|la r.*siliation)",
	      "gl" => "(Houbo un erro ao dar de alta|Produciuse un erro ao dar de baixa) a subscrición",
	      "he" => "שגיאה (ברישום|בביטול המנוי)",
	      "hr" => "Gre.*ka kod (pretpla.*ivanja|Odjavljivanja)",
	      "hu" => "Hiba a (feliratkoz.*skor|t*rl*sn*l)",
	      "ia" => "Error in (abonar|disabonar)",
	      "it" => "Errore durante (l'iscrizione|la cancellazione)",
	      # | recode EUC-JP..utf8
	      "ja" => "(��|��)���³���Υ��顼|(入|退)会手続きのエラー",
	      # | recode EUC-KR..utf8
	      "ko" => "(����|Ż��) ����|(가입|탈퇴) 에러",
	      "lt" => "Nes.*kmingai u.*sisakin.*jo|Klaida atsisakant",
	      "nl" => "Fout bij (het aanmelden|afmelden)",
	      "no" => "Feil under (p.*melding|utmelding)",
	      "pl" => "B.*dy przy (za|wy)pisywaniu",
	      "pt" => "Erro (inscrevendo|ao cancelar a inscri.*o)",
	      "pt_BR" => "Erro ao (inscrever|descadastrar)",
	      "ro" => "Eroare la (abonare|dezabonare)",
	      # | recode koi8-r..utf8
	      "ru" => "��������� �� ����|������ �������� ��������|Подписаны НЕ были|Ошибка удаления подписки",
	      "sk" => "Chyba pri (prihlasovan.*|odhlasovan.*)",
	      "sl" => "Napaka pri (prijavljanju|odjavi)",
	      "sr" => "Грешка при (у|uc)пису",
	      "sv" => "Fel under (anm.*lan|avanm.*lan)",
	      "tr" => "(.*ye yaparken|.*yelikten .*kar.*l.*rken) hata oldu",
	      "uk" => "Помилка (при спробі|видалення) підписки",
	      "vi" => "Lỗi đăng ký|Lỗi bỏ đăng ký",
	      "zh_CN" => "错误(取)?订阅",
	      "zh_TW" => "訂閱失敗|退訂時出錯",
	     },
	     # grep -r -A 1 'msgid "No such list .*"' messages/*
	     "no_such_list" =>
	     {
	      "C" => "No such list",
	      "ar" => "لا يوجد قائمة بالإسم",
	      "ca" => "La llista .* no existeix",
	      "cs" => "Nenalezl jsem konferenci",
	      "da" => "Listen findes ikke",
	      "de" => "(Keine Liste mit Namen .* vorhanden|Liste nicht vorhanden)",
	      "es" => "(La lista .* no existe|No existe tal lista)",
	      "et" => "(Sellist listi pole|Selle nimega listi pole)",
	      "eu" => "(zerrendarik ez dago|Zerrenda ezezaguna)",
	      "fi" => "(Listaa .* ei ole olemassa|Lista on jo olemassa)",
	      "fr" => "(Liste inexistante|Liste introuvable)",
	      "gl" => "(A rolda .* non existe|Non existe esa rolda)",
	      "he" => "(אין רשימה בשם|אין כזו רשימה)",
	      "hr" => "Takva lista ne postoji <em>%(safelistname)s</em>",
	      "hu" => "Nincs .* nev.* lista",
	      "ia" => "(Le lista .* non existe|Nulle tal lista)",
	      "it" => "Non esiste .*la lista",
	      # | recode EUC-JP..utf8
	      "ja" => "�Ȥ����ꥹ�ȤϤ���ޤ���|というリストはありません",
	      # | recode EUC-KR..utf8
	      "ko" => "��� ���ϸ� ����Ʈ�� �������� �ʽ��ϴ�|라는 메일링 리스트가 존재하지 않습니다.",
	      "lt" => "N.*ra forumo",
	      "nl" => "Er is geen lijst met de naam",
	      "no" => "Listen finnes ikke",
	      "pl" => "Nie znaleziono listy|Nie ma takiej listy",
	      "pt" => "N.*o existe essa lista|Lista inexistente",
	      "pt_BR" => "Lista .*inexistente",
	      "ro" => "Nu exist.* lista|Lista aceata nu exist.*",
	      # | recode koi8-r..utf8
	      "ru" => "������ �������� .*�� ����������|Список рассылки .*не существует",
	      "sk" => "Neznáma .*konferencia",
	      "sl" => "Seznam .*ne obstaja",
	      "sr" => "Нема листе",
	      "sv" => "Listan finns inte",
	      "tr" => "ad.*nda bir liste yok",
	      "uk" => "Список розсилки .*не існує",
	      "vi" => "Không có hộp thư (chung|như vậy)",
	      "zh_CN" => "没有类似的列表|没有这个列表",
	      "zh_TW" => "(沒有.*這個|無此)論壇",
	     },
	     # head -n 2 templates/*/admindbsummary.html
	     # grep -r -A 1 'msgid "There are no pending requests."' messages/*
	     "pending_req" =>
	     {
	      "C" => "There are no pending requests",
	      "ar" => "تحتوي هذه الصفحة على تلخيص للطلبات الإشرافية|لا يوجد طلبات معلقة",
	      "ca" => "Aquesta p.*gina cont.* un sumari del conjunt actual de peticions administratives|No hi ha peticions pendents",
	      "cs" => "P.*ehled po.*adavk.* pro konferenci|.*dn.* po.*adavky ne.*ekaj.* na vy.*zen.*",
	      "da" => "Her finder du en oversigt over anmodninger der skal vurderes for maillisten|Der venter ingen anmodninger",
	      "de" => "Diese Seite zeigt eine .*bersicht der gegenw.*rtigen administrativen|Keine unbearbeiteten Anfragen",
	      "en" => "This page contains a summary of the current set of administrative",
	      "es" => "Esta página contiene un sumario de las solicitudes administrativas que|No hay peticiones pendientes",
	      "et" => "Sellel lehel on ülevaade kõigist||Taotlusi pole",
	      "eu" => "Orri honetan .* posta zerrendan|Ez dago eskaerarik zain",
	      "fi" => "Tällä sivulla on lista toimiasi vaativista|Ei odottavia pyynt.*j.*",
	      "fr" => "Cette page contient un r.*sum.* de l'ensemble des requ.*tes|Pas de requ.*tes en instance",
	      "gl" => "Esta páxina cont.*n un sumario das solicitudes administrativas que|Non hai ningunha solicitude pendente",
	      "he" => "עמוד זה מכיל סיכום של קבוצת כל הבקשות המנהלתיות שדורשות|אין בקשות ממתינות",
	      "hr" => "Ova stranica sadr.*i sa.*etak trenutnog skupa administrativnih zahtjeva|Nema zahtjeva na .*ekanju",
	      "hu" => "Ezen az oldalon .* levelezõlistához.* tartozó beavatkozásra|Nincsen beavatkoz.*sra v.*r.* teend.*",
	      "ia" => "Iste pagina contine un summario del collection del requestas|Il non ha requestas pendente",
	      "it" => "Questa pagina contiene la lista delle richieste amministrative|Non ci sono richieste in attesa",
	      "ja" => "���Υڡ�����|このページは|��α��ο����Ϥ���ޤ���|保留中の申請はありません",
	      "ko" => "�� �������� .* ���ϸ� ����Ʈ|이 페이지는 .* 메일링 리스트|������� ��û�� �����ϴ�|대기중인 요청이 없습니다",
	      "lt" => "Sprendimo laukian.*i.* lai.*k.* santrauka|There are no pending requests",
	      "nl" => "Deze pagina toont een overzicht van alle administratieve verzoeken m.b.t. de .* maillijst die wachten op uw goedkeuring|Er zijn geen wachtende verzoeken",
	      "no" => "Her finner du en oversikt over foresp.*rsler som skal vurderes for epostlisten|Det venter ingen foresp.*rsler eller s.*knader",
	      "pl" => "This page contains a summary of the current set of administrative|Brak skolejkowanych zada.*",
	      "pt" => "Esta p.*gina cont.*m um sum.*rio dos pedidos administrativos da lista|N.*o h.* pedidos pendentes",
	      "pt_BR" => "Esta p.*gina cont.*m um resumo do conjunto atual de requisi.*es|N.*o existem requisi.*es pendentes",
	      "ro" => "Aceast.* pagin.* con.*ine un sumar al setului curent de cereri administrative|Nu sunt cereri .*n a.*teptare",
	      "ru" => "��� �������� �������� ������� ������ ��������� ��������� ����������������|Эта страница содержит сводный список требующих обработки административных|��� ��������, ��������� ���������|Нет запросов, требующих обработки",
	      "sk" => "Preh.*ad po.*iadaviek pre konferenciu|.*iadne .*iadosti ne.*akaj.* na spracovanie",
	      "sl" => "Ta stran vsebuje povzetek trenutnih skrbni.*kih zahtev, ki .*akajo|Ni .*akajo.*ih zahtev",
	      "sr" => "﻿Ова страна садржи преглед тренутних услова за ваше укључење у листу слања|Нема захтјева на чекању",
	      "sv" => "H.*r finns en .*versikt .*ver f.*rfr.*gningar som ska avg.*ras f.*r e-postlistan|Inga ans.*kningar v.*ntar",
	      "tr" => "Bu sayfa|Bekleyen istek yok",
	      "uk" => "Ця сторінка містить загальний список адміністративних запитів|Відсутні запити, що очікують рішень",
	      "vi" => "Trang này chứa bản tóm tắt các yêu cầu quản trị cần thiết bạn tán thành cho|Không có yêu cầu bị hoãn nào",
	      "zh_CN" => "此页面包含.*邮件列表|没有挂起的请求",
	      "zh_TW" => "沒有待決的事項",
	     },
	     # TODO: get strings from older Mailman (pre 2.1) containing this one
	     "headline_subscr" =>
	     {
	      "en" => "subscription",
	      "da" => "medlemskab",
	     },
	     # TODO: get strings from older Mailman (pre 2.1) containing this one
	     "held_for_approval" =>
	     {
	      "en" => "held for approval",
	     },
	     # grep -r -A 1 'msgid "Already a member"' messages/*
	     "already_member" =>
	     {
	      "C" => "Already a member",
	      "ar" => "مشترك أصلاً",
	      "ca" => "Ja ets membre",
	      "cs" => "Je ji.* .*astn.*kem",
	      "da" => "Allerede medlem",
	      "de" => "Bereits Mitglied",
	      "es" => "Ya est.* suscrito",
	      "et" => "On juba liige",
	      "eu" => "Dagoeneko harpidetuta",
	      "fi" => "Jo j.*sen",
	      "fr" => "D.*j.* abonn.*",
	      "gl" => "Xa está subscrito",
	      "he" => "הנו כבר מנוי",
	      "hr" => "Ve.* je .*lan",
	      "hu" => "M.*r tag",
	      "ia" => "Ja es un membro",
	      "it" => "Gi.* iscritto",
	      # | recode EUC-JP..utf8
	      "ja" => "���˲���Ǥ�|既に会員です",
	      # | recode EUC-KR..utf8
	      "ko" => "�̹� ȸ���Դϴ�|이미 회원입니다",
	      "lt" => "Jau dalyvis",
	      "nl" => "Is al lid",
	      "no" => "Allerede medlem",
	      "pl" => "Ju.* jest zapisany",
	      "pt" => "J.* .* um membro",
	      "pt_BR" => "J.* .* um membro",
	      "ro" => "Este membru deja",
	      # | recode koi8-r..utf8
	      "ru" => "��� �������� �����������|Уже является подписчиком",
	      "sk" => "Je už účastníkom",
	      "sl" => "Je .*e .*lan",
	      "sr" => "Корисник је већ учлањен.",
	      "sv" => "Redan medlem",
	      "tr" => "Zaten listeye .*ye",
	      "uk" => "Вже є учасником",
	      "vi" => "Đã thành viên",
	      "zh_CN" => "已经是成员了",
	      "zh_TW" => "已是訂戶",
	     },
	    );

    my $t = $translations{$key};
    die "INTERNAL ERROR: Unknown translation key '$key'\n"
	    unless defined $t;
    return "(?i)(" . join("|", values %{$t}) . ")";
}

sub guess_charset {
    my ($charset, $text) = @_;

    # Mislabeling Shift JIS as ISO 2022 is a very common mistake.
    if ($charset =~ /^iso-2022-jp/i && $text =~ /[\x80-\x9f]/) {
	return "Shift_JIS";
    }
    return $charset;
}

sub decode_rfc2047_qp {
    my ($charset, $encoded_word) = @_;
    my $text = $encoded_word;
    $text =~ s/_/ /g;
    $text = MIME::QuotedPrint::decode($text);
    $charset = guess_charset($charset, $text);
    eval { $text = Encode::decode($charset, $text) };
    return defined $text ? $text : $encoded_word;
}

sub decode_rfc2047_base64 {
    my ($charset, $encoded_word) = @_;
    my $text = MIME::QuotedPrint::decode_base64($encoded_word);
    $charset = guess_charset($charset, $text);
    eval { $text = Encode::decode($charset, $text) };
    return defined $text ? $text : $encoded_word;
}

sub decode_rfc2047 {
    my ($hdr, $config) = @_;

    # Bugs: Decodes invalid tokens, where the encoded word is
    # concatenated with other letters, e.g.  foo=?utf-8?q?=A0=F8?=
    # Also decodes base64 encoded words which are doubly encoded with
    # quoted-printable.

    $hdr =~ s/=\?([^? ]+)\?q\?([^? ]*)\?=/
	    decode_rfc2047_qp($1, $2)/ieg;
    $hdr =~ s/=\?([^? ]+)\?b\?([^? ]*)\?=/
	    decode_rfc2047_base64($1, $2)/ieg;

    return degrade_charset($hdr, $config->{unprintable});
}

sub degrade_charset {
    my ($text, $unprintable) = @_;

    # Handle unencoded Shift JIS (Japanese) text.  The input text is
    # either raw data from the message, or Unicode, in which case it
    # will not contain these code points.  This discrimates slightly
    # against users of Windows-1252, which has curved quotes at 0x82
    # (0x81 is unassigned).

    if ($text =~ /[\x81\x82]/) {
	eval { $text = Encode::decode("Shift_JIS", $text) };
    }

    # This may look a bit silly.  We first encode to the character set
    # of our terminal.  If it is a limited character set such as
    # Latin1, Chinese glyphs are converted into e.g. "&#x41a;", while
    # "n with tilde" will be a single glyph.  We then convert this
    # back to a Unicode string so that the length is right (number of
    # glyphs, not octets) for Text::Reform.  Finally, when the Unicode
    # string is printed to the screen, the binmode directive for
    # STDOUT tells Perl to once more translate it into the terminal's
    # character set.

    eval {
	$text = Encode::decode($term_encoding,
			       Encode::encode($term_encoding, $text,
					      Encode::FB_HTMLCREF))
	    };

    # The built-in formats for unprintable glyphs are ugly, and to be
    # allowed to specify a code ref which returns our preferred format
    # directly, we need to require Encode version 2.10, which feels a
    # bit unnecessary.

    if (defined $config && $unprintable eq "unicode") {
	$text =~ s/&\#(\d+);/sprintf("<U+%04x>", $1)/ge;
    } else {
	$text =~ s/&\#\d+;/<?>/g;
    }

    # Get rid of ESC sequences which may cause havoc with the
    # terminal, we only keep TAB and LF.  Also removes control
    # characters with high bit set, 127-159, which are unallocated in
    # Unicode.

    $text =~ s/([\x00-\x08\x0b-\x1f\x7f-\x9f])/sprintf("<%02x>", ord($1))/eg;

    return $text;
}


sub parse_approval {
    my ($mmver, $config, $parse, $data) = @_;
    my ($from, $reason, $subject, $id, $body, $headers);

    $parse->get_tag ("tr") || die;	# From:
    $parse->get_tag ("td") || die;
    $parse->get_tag ("td") || die;
    $from = $parse->get_trimmed_text("/td");

    if ($mmver eq "1.2") {
	$parse->get_tag ("tr") || die;	# Reason:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die; 
	$reason = $parse->get_trimmed_text("/td");
	$parse->get_tag ("tr") || die;	# Subject:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$subject = $parse->get_trimmed_text("/td");
    } else {
	$parse->get_tag ("tr") || die;	# Subject:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die; 
	$subject = $parse->get_trimmed_text("/td");
	$parse->get_tag ("tr") || die;	# Reason:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$reason = $parse->get_trimmed_text("/td");
    }
    $parse->get_tag ("tr") || die;	# Action:
    my $tag = $parse->get_tag ("input") || die;
    $id = $tag->[1]{"name"};

    $data->{$id} = { "from" => decode_rfc2047($from, $config),
		     "subject" => $subject,
		     "reason" => $reason };

    $parse->get_tag ("tr") || die; # Reject _or_ Preserve message
    if ($mmver ge "2.0") {
	$parse->get_tag ("tr") || die;    # forward
	$parse->get_tag ("tr") || die;    # Reject
    }
    $parse->get_tag ("td") || die;
    $parse->get_tag ("td") || die;
    $data->{$id}->{"rejreason"} = $parse->get_trimmed_text("/td") || die;


    $parse->get_tag ("tr") || die; # Message Excerpt _or_ Headers
    $parse->get_tag ("td") || die;
    $parse->get_tag ("td") || die;
    $headers = $parse->get_text("/td");

    # We handle spam score headers on the formats:
    #   X-spam-score: *****
    #   X-spam-score: 4.23 (****)
    #
    # The name of the header is flexible.
    my $header_re = $config->{"spamheader"} || 'X-\S*spam-?(?:level|score)';

    # Extract all spam score headers, and pick the max value:
    my $spamscore = 0;
    while ($headers =~ /^$header_re:\s+
	                   (-?\d+\.\d+\s+)?
                           \(?
                             ((\S)\3*)
                           (?:\s|\)|$)/xgim) {
	my $score = defined $1 ? int($1): length($2);
	$spamscore = $score if $score > $spamscore;
    }
    $data->{$id}->{"spamscore"} = $spamscore;
    $data->{$id}->{"date"} = "<no date>";
    $data->{$id}->{"date"} = $1
	    if $headers =~ /^Date:\s+(.*)$/m;
    if ($mmver ge "2.0") {
	$parse->get_tag ("tr") || die;  # Message Excerpt
	$parse->get_tag ("td") || die;
	$parse->get_tag ("textarea") || die;
	$body = $parse->get_text("/textarea");
    } else {
	$headers =~ s/\n\n//s;
	$body = $POSTMATCH;
	$headers = $PREMATCH;
    }
    $headers =~ s/\n(\s)/$1/g; # Header folding
    $headers =~ s/^\s+//;
    $data->{$id}->{"headers"} = $headers;

    # Mailman decodes Subject itself, but at least version 2.0 and 2.1
    # screw up non-ASCII characters, so we get the raw value from the
    # headers instead.
    if ($headers =~ /^Subject:\s*(.*)\s*$/mi) {
	$subject = $1;
    }
    if ($subject =~ /[\x80-\xff]/) {
	$subject .= " [unencoded]";
    }

    $data->{$id}->{"subject"} = decode_rfc2047($subject, $config);

    $body .= "\n" unless $body =~ /\n$/;
    $data->{$id}->{"body"} = $body;

    return ($mmver);
}

sub set_param_values {
    my ($mmver, $data) = @_;

    if ($mmver ge "2.0") {
	$data->{"global"}{"actions"} = { "a" => 1,
					 "r" => 2,
					 "d" => 3,
					 "sa" => 4, # subscribe approve
					 "sr" => 2, # subscribe reject
					 "sd" => 3, # subsribe discard
				     };
    } else {
	$data->{"global"}{"actions"} = { "a" => 0,
					 "r" => 1,
					 "d" => 2,
					 "sa" => 1, # subscribe approve
					 "sr" => 0, # subscribe reject
				     };
    }
}

sub read_config {
    my ($file) = @_;

    my %cur = map { $_ => []; }
	    qw (not_spam_if_from
		not_spam_if_subject
		discard_if_from
		discard_if_subject
		discard_if_reason);
    my $pattern_keywords = join ("|", keys %cur);

    # Defaults:
    $cur{user} = $cur{password} = $cur{action} = $cur{default} = "";
    $cur{confirm} = 1;
    $cur{unprintable} = "questionmark";

    my $conf = {};
    my $line = "";
    my $count = 0;
    my $lineno = 0;

    my %act = ("approve" => "a", "discard" => "d",
	       "reject" => "r", "skip" => "s", "none" => "");
    my %sact = ("accept" => "a", "discard" => "d", 
		"reject" => "r", "skip" => "s", "none" => "");

    return undef unless open (CONF, $file);
    while (<CONF>) {
	++$lineno;
	chomp;
	s/\r$//;
	s/\s+$//;		# trailing whitespace is "always" unintended
	next if /^\s*\#/;
	s/^\s+// if $line;	# remove leading whitespace after continuation
	if (/\\$/) {
	    $line .= $PREMATCH;
	    next;
	}
	$line .= $_;
	$line =~ s/^\s+//;
	next if /^$/;
	if ($line =~ /^username\s+/i) {
	    $cur{user} = unquote($POSTMATCH);
	    if ($cur{user} !~ /^[a-z0-9._+-]+\@[a-z0-9.-]+$/) {
		print STDERR "$file:$lineno: Illegal username: '$cur{user}'\n";
		exit 1;
	    }
	} elsif ($line =~ /^password\s+/i) {
	    $cur{password} = unquote($POSTMATCH);
	} elsif ($line =~ /^spamlevel\s+/i) {
	    $cur{spamlevel} = unquote($POSTMATCH);
	    if ($cur{spamlevel} =~ /^(\d+)\s*$/) {
		$cur{spamlevel} = $1;
	    } else {
		print STDERR "$file:$lineno: Illegal value: '$cur{spamlevel}'\n";
		print STDERR "choose a positive numeric value\n";
		exit 1;
	    }
	} elsif ($line =~ /^(confirm|meta_member_support)\s+/i) {
	    my ($key, $value) = (lc($1), unquote($POSTMATCH));
	    if ($value eq "yes") {
		$value = 1;
	    } elsif ($value eq "no") {
		$value = undef;
	    } else {
		print STDERR "$file:$lineno: Illegal value: '$value\n";
		print STDERR "choose one of yes or no\n";
		exit 1;
	    }
	    $cur{$key} = $value;
	} elsif ($line =~ /^(action|default)\s+/i) {
	    my ($key, $value) = (lc($1), unquote($POSTMATCH));
	    unless (exists $act{$value}) {
		print STDERR "$file:$lineno: Illegal value: '$value\n";
		print STDERR "choose one of ",
                             join (", ", sort keys %act), "\n";
		exit 1;
	    }
	    $cur{$key} = $act{$value};
	} elsif ($line =~ /^adminurl\s+/i) {
	    $cur{adminurl} = unquote($POSTMATCH);
	    $cur{adminurl} = undef if $cur{adminurl} eq "NONE";
	} elsif ($line =~ /^log\s+/i) {
	    $cur{logfile} = expand_pathname(unquote($POSTMATCH));
	} elsif ($line =~ /^dumpdir\s+/i) {
	    $cur{dumpdir} = expand_pathname(unquote($POSTMATCH));
	    mkdir($cur{dumpdir}) if (defined $cur{dumpdir});
	} elsif ($line =~ /^subscription_(action|default)\s+/) {
	    my $key = "sub" . lc($1);
	    my $value = unquote($POSTMATCH);
	    unless (exists $sact{$value}) {
		print STDERR "$file:$lineno: Illegal value: '$value'\n";
		print STDERR "choose one of ",
		             join (", ", sort keys %sact), "\n";
		exit 1;
	    }
	    $cur{$key} = $sact{$value};
	} elsif ($line =~ /^($pattern_keywords)\s+/o) {
	    my $key = $1;
	    my $val = $POSTMATCH;
	    $val =~ s/\s+$//;
	    if ($val =~ /^"(.*)"$/) {
		$val = $1;
		$val =~ s/\\"/"/g;
		$val =~ s/\\\\/\\/g;
	    }
	    push($cur{$key}, ($val eq "NONE") ? undef : $val);
	} elsif ($line =~ /^spamheader\s+/) {
	    $cur{spamheader} = unquote($POSTMATCH);
	    unless ($cur{spamheader} =~ /^[\w-]+$/) {
		print STDERR "$file:$lineno: Illegal header name: ".
			"'$cur{spamheader}'\n";
		exit 1;
	    }
	    $cur{spamheader} = undef if $cur{spamheader} eq "default";
	} elsif ($line =~ /^([^@ \t]+@[^@])+\s*/) {
	    my %copy = %cur;
	    $copy{order} = ++$count;
	    $conf->{$line} = \%copy;
	} elsif ($line =~ /^unprintable\s+/) {
	    $cur{unprintable} = unquote($POSTMATCH);
	    unless ($cur{unprintable} =~ /^(questionmark|unicode)$/) {
		print STDERR "$file:$lineno: Illegal format for ".
			"unprintable characters: '$cur{unprintable}'\n";
		exit 1;
	    }
	} else {
	    print STDERR "$file:$lineno: Syntax error: '$line'\n";
	    exit 1;
	}
	$line = "";
    }
    close (CONF);
    return $conf;
}

sub unquote {
    my ($val) = @_;
    $val =~ s/\s+$//;
    if ($val =~ /^"(.*)"$/) {
	$val = $1;
	$val =~ s/\\"/"/g;
	$val =~ s/\\\\/\\/g;
    }
    return ($val);
}

sub expand_pathname {
    my ($pathname) = @_;

    $pathname =~ s,^\$HOME/,$ENV{'HOME'}/,;
    $pathname =~ s,^~/,$ENV{'HOME'}/,;
    $pathname =~ s,^~(\w+)/,(getpwnam($1))[7]."/",e;
    if ($pathname =~ /^M:/i) {
	$pathname =~ s,\\,/,g;
	$pathname =~ s,^M:,$ENV{'HOME'},;
    }
    $pathname = undef if $pathname eq "none";
    return $pathname;
}


sub prompt_for_config {
    my ($rc) = @_;

    print "No configuration file found: $rc\n";
    my $ans = prompt ("Do you want to create one? [yes] ");
    print "\n";
    if ($ans !~ /^\s*(|y|yes|j|ja)\s*$/i) {
	print "I take that as a no.  Goodbye!\n";
	return undef;
    }
    umask 077;
    unless (open (RC, ">$rc")) {
	print STDERR "$rc: $!\n";
	return undef;
    }
    my $user = prompt ("Enter Mailman username: ");
    print "\n";
    print RC "username $user\r\n";
    my $pass = prompt_password("Enter Mailman password: ");
    print "\n";
    $pass =~ s/"/\\"/g;
    print RC "password \"$pass\"\r\n";

    print <<END;
Listadmin can discard messages with a high spam score automatically.
A value in the interval 5 to 12 is recommended.
END
    my $spam = prompt ("What threshold do you want? [8]");
    print "\n";
    $spam =~ s/\s*//g;
    $spam ||= "8";
    if ($spam =~ /^\d+$/) {
	print RC "spamlevel $spam\r\n";
    } else {
	print "No automatic discard will be done.\n";
    }
    my $extra = <<END;

# If you uncomment the following you will only have to press Return
# to discard a message:
#
# default discard

# Uncomment the following to get a terse transaction log:
#
# log "~/.listadmin.log"

END
    $extra =~ s/\n/\r\n/g;
    print RC $extra;

    print <<END;
Now enter the addresses of the lists you maintain.  End with an empty
line.
END
    my $list;
    do {
	$list = prompt ("> ");
	print "\n";
	$list =~ s/\s*//g if $list;
	print RC "$list\r\n" if $list;
    } while ($list);
    close (RC);
    print <<END;

The configuration has been saved in $rc.
You can edit this file with an ordinary text editor, such as Notepad,
Pico, or Emacs.  To read about all the configuration options, run
'man listadmin'.

END
    return 1;
}

sub commit_changes {
    my ($list, $user, $pw, $url, $change, $msgs, $logfile) = @_;

    my $baseurl = mailman_url ($list, $url);
    my $action = $msgs->{"global"}{"actions"};
    my $changes = 0;
    my $update_total = scalar (keys %{$change});
    my $update_count = 0;
    my $params = mailman_params ($user, $pw);

    my $log = log_timestamp ($list);
    # Expand {list}, {subdomain} and {domain}, if there is something to expand
    $logfile = mailman_url($list, $logfile) if $logfile;

    for my $id (sort { $a <=> $b } keys %{$change}) {
	my ($what, $text) = @{$change->{$id}};
	$params->{$id} = $action->{$what};
	unless ($what =~ /^s[ard]$/) {
	    # we don't log subscription approval or rejects
	    $log .= sprintf ("%s D:[%s] F:[%s] S:[%s]\n",
			     $what,
			     $msgs->{$id}{"date"},
			     $msgs->{$id}{"from"},
			     $msgs->{$id}{"subject"});
	}
	if ($what =~ /^s?r$/) {
	    $params->{"comment-$id"} = $text;
	}
	++$changes;

	# HTTP does not specify a maximum size for a POST request, so
	# we could do this as one request.  However, Apache is usually
	# set up to close the connection after the CGI script has run
	# for 5 minutes, so we reduce the size of each request to be
	# nice to slow servers.

	if ($changes >= 100) {
	    $update_count += $changes;
	    printf("sending %d updates to server, %d left    \r",
		   $changes, $update_total - $update_count);
	    submit_http ($baseurl, $params, $log, $logfile);
	    $log = log_timestamp ($list);
	    $changes = 0;
	    $params = mailman_params ($user, $pw);
	    
	    # even if time has run out, we will always submit at least
	    # one batch of data.
	    if (time > $time_limit) {
		print "\nTime's up, won't submit the other changes\n";
		last;
	    }
	}
    }
    submit_http ($baseurl, $params, $log, $logfile)
	    if $changes;
    print (" " x 72, "\r") if $update_count > 0;
}

sub log_timestamp {
    my $list = shift;

    my ($sec, $min, $hour, $mday, $mon, $year) = (localtime (time))[0..5];
    return (sprintf ("submitting %s %04d-%02d-%02dT%02d:%02d:%02d\n",
		     $list, $year+1900, $mon+1, $mday, $hour, $min, $sec));
}

sub add_subscribers {
    my ($list, $config, $mail, @addresses) = @_;

    die unless @addresses;

    fetch_meta_members($list, $config);

    my %params = (username => $config->{user},
		  adminpw => $config->{password},
		  subscribe_or_invite => 0,                # Mailman 2.x
		  send_notifications_to_list_owner => 0,   # Mailman 2.x
		  send_welcome_message_to_this_batch => 0, # Mailman 2.x
		  send_welcome_msg_to_this_batch => 0,     # Mailman 1.2
		  meta_members => $config->{meta_members}, # Mailman 1.2
		  subscribees => join("\n", @addresses));
    my $url = mailman_url($list, $config->{adminurl}, "", "members");
    my $resp = $ua->post($url, \%params);
    return $resp->status_line unless $resp->is_success;

    my $result = parse_subscribe_response($resp->content);

    if (!$mail) {
	my %left = map { $_ => 1 } @addresses;
	for my $failed (keys %{$result}) {
	    unless ($result->{$failed} =~ get_trans_re("already_member")) {
		delete $left{$failed};
	    }
	}
	@addresses = keys %left;
    } else {
	# We only need to reset "nomail" on the users who already were
	# members.
	@addresses = ();
	for my $failed (keys %{$result}) {
	    if ($result->{$failed} =~ get_trans_re("already_member")) {
		push(@addresses, $failed);
	    }
	}
    }
    if (@addresses) {
	%params = (username => $config->{user},
		   adminpw => $config->{password},
		   user => \@addresses,
		   meta_members => $config->{meta_members}, # Mailman 1.2
		   setmemberopts_btn => "submit");	# Mailman 2.x
	for my $a (@addresses) {
	    $params{$a . "_nomail"} = "on" unless $mail;
	    $params{$a . "_subscribed"} = "on";		# Mailman 1.2
	}
	$resp = $ua->post($url, \%params);
	return $resp->status_line unless $resp->is_success;
    }

    return $result;
}

sub remove_subscribers {
    my ($list, $config, @addresses) = @_;

    fetch_meta_members($list, $config);

    my $url = mailman_url($list, $config->{adminurl}, "", "members");

    # In Mailman 1.2, unsubscription happens when an address is
    # mentioned in "user" without a corresponding
    # "$address_subscribed" parameter
    my %params = (username => $config->{user},
		  adminpw => $config->{password},
		  setmemberopts_btn => "submit",	# Mailman 2.x
		  meta_members => $config->{meta_members}, # Mailman 1.2
		  user => \@addresses);
    for my $a (@addresses) {
	$params{$a . "_unsub"} = "on";			# Mailman 2.x
    }
    my $resp = $ua->post($url, \%params);
    return $resp->status_line unless $resp->is_success;

    return parse_subscribe_response($resp->content);
}


sub parse_subscribe_response {
    my ($page) = @_;

    # Normalise, to make parsing easier (Hack!)
    $page =~ s/<h3\>/\<h5\>/ ;
    $page =~ s/<\/h3\>/\<\/h5\>/;

    # In Mailman 1.2 and 2.0, you will not get an explicit success
    # report when removing subscribers, so we only return the
    # failures since the successes can be inferred anyway.

    my %failure = ();

    my $parse = HTML::TokeParser->new(\$page) || die;

    while ($parse->get_tag ("h5")) {
	my $h5 = $parse->get_text ("/h5");

	$parse->get_tag ("ul") || die;
	my $ul = $parse->get_text ("/ul") || die;

	if ($h5 =~ get_trans_re("subscr_success")) {
	    # hooray!
	} elsif ($h5 =~ get_trans_re("subscr_error")) {
	    for (split(/\n/, $ul)) {
		chomp;
		if (/^\s*(.*?)\s*--\s*(.*)/) {
		    $failure{$1} = $2;
		}
	    }
	} else {
	    $ul =~ s/\n/\n\t/g;
	    print STDERR "You have an unusual Mailman output.  Please mail ".
		    "this message to\n$maintainer\n:\n".
		    "\t[$h5]\n\t[$ul]\nThanks!\n";
	}
	$parse->get_tag ("p") || die;
    }

    return \%failure;
}

sub list_subscribers {
    my ($list, $config) = @_;

    fetch_meta_members($list, $config);
    my $url = mailman_url($list, $config->{adminurl}, "", "members");
    my %params = (username => $config->{user},
		  adminpw => $config->{password},
		  meta_members => $config->{meta_members},
		  chunk => 0);
    my $resp = $ua->post($url, \%params);
    unless ($resp->is_success) {
	print "$url: ", $resp->status_line, "\n";
	return ();
    }

    my @addresses = ();
    my ($parse, $page, $tag);

 member_letter:
    for my $letter ("a" .. "z") {
	my $chunk = 0;

	$params{chunk} = $chunk;

	# Mailman 2.x specifically looks at QUERY_STRING, so chunk and
	# letter can't be parameters to POST.  However, Mailman 1.x
	# only looks at chunk in the POST parameters.
	$resp = $ua->post("$url?letter=$letter&chunk=$chunk", \%params)
		unless $letter eq "a";

	while ($resp->is_success) {
	    $page = $resp->content;
	    $parse = HTML::TokeParser->new(\$page);
	    my $count = 0;
	    my $repeated = 0;
	    my $later_letter = 0;
	    while ($tag = $parse->get_tag("input")) {
		my $attr = $tag->[1];
		if ($attr->{type} =~ /^hidden$/i &&
		    $attr->{name} =~ /^user$/i) {
		    ++$count;
		    my $address = $attr->{value};
		    unless ($address =~ /\@/) {
			# Mailman 2.x adds URL-encoding
			$address =~ s/%([0-9a-fA-F]{2})/sprintf("%c", hex($1))/ge;
		    }
		    ++$later_letter if lc(substr($address, 0, 1)) gt $letter;
		    if (grep { $_ eq $address } @addresses) {
			++$repeated;
		    } else {
			push(@addresses, $address);
		    }
		}
	    }
	    last if $count == 0;

	    # In Mailman 1.x, "letter" is a no-op, so $later_letter
	    # will ~always be true and should be ignored.  Increase
	    # chunk until we see repeats.

	    # In Mailman 2.x, we need to iterate through both letter
	    # and chunk, but if the list has few members, they will
	    # all be listed and letter and chunk are ignored.  Also,
	    # if there are no members for a given letter, the whole
	    # list will be returned.

	    if ($repeated) {
		last member_letter if $later_letter;
		next member_letter;
	    }

	    # The maximum number of addresses on each page can be
	    # configured, by default it is set to 30, but it could in
	    # theory be less.  To save time, we assume that we have
	    # all the members if we got less than 20 addresses.
	    next member_letter if $count < 20;

	    ++$chunk; $params{chunk} = $chunk;
	    $resp = $ua->post("$url?letter=$letter&chunk=$chunk", \%params);
	}
    }
    if ($config->{meta_members}) {
	push(@addresses, split(/\n+/, $config->{meta_members}));
    }
    return @addresses;
}

# This code is only useful on the patched Mailman 1.2 installation at
# UiO.  Notice that it uses GET without any parameters to fetch the
# page, since otherwise it will clear the meta members.
# Unfortunately, this means we need to use cookies to log in, and this
# requires a new Perl module, WWW::Mechanize.  Since this is such a
# site specific feature, we hide the requirement so listadmin runs
# even without the module.

sub fetch_meta_members {
    my ($list, $config) = @_;

    return if defined $config->{meta_members}; # already fetched
    return unless $config->{meta_member_support} || $list =~ /\buio\.no$/i;

    # We will only attempt this once, so make a note we've tried.
    $config->{meta_members} = "";

    unless (eval "require WWW::Mechanize; 1") {
	print "WARNING: Meta members may be removed, install WWW::Mechanize\n";
	return;
    }

    my $agent = WWW::Mechanize->new(autocheck => 1);
    $agent->get(mailman_url($list, $config->{adminurl}));
    $agent->submit_form(fields => { username => $config->{user},
				    adminpw => $config->{password}});

    $agent->get(mailman_url($list, $config->{adminurl}, "", "members"));

    my $page = $agent->content();
    my $parse = HTML::TokeParser->new(\$page);
    my $tag = $parse->get_tag("textarea");
    $tag = $parse->get_tag("textarea");
    return unless defined $tag; # silently ignore the failure

    if ($tag->[1]->{name} eq "meta_members") {
	$config->{meta_members} = $parse->get_trimmed_text("/textarea");
    }
}

sub remove_matching_subscribers {
    my ($list, $config, $pattern) = @_;
    my @addresses = list_subscribers($list, $config);
    if (defined($pattern) and $pattern ne "") {
	@addresses = grep { /$pattern/ } @addresses;
    }
    my $msg = remove_subscribers($list, $config, @addresses);
    if ($msg eq "OK") {
	print "Removed:\n  ", join("\n  ", @addresses), "\n";
    } else {
	print $msg, "\n";
    }
}

sub read_address_file {
    my ($file, $assert_nonempty) = @_;
    my @list = ();
    open(F, $file) || die "$file: $!\n";
    while (<F>) {
	s/(^|\s)\#.*//;
	s/^\s+//;
	s/\s+$//;
	next if /^$/;
	push(@list, $_);
    }

    die "$file: no lines, aborting\n" if $assert_nonempty && @list == 0;
    return @list;
}

sub submit_http {
    my ($url, $params, $log, $logfile) = @_;

    my $opened;
    if ($logfile) {
	if (open (LOG, ">>$logfile")) {
	    LOG->autoflush(1);
	    # Perhaps we should force the encoding to US-ASCII
	    # instead, but I think this is more DWIM compliant.
	    binmode LOG, ":encoding($term_encoding)";
	    $opened = 1;
	    local $SIG{__WARN__} = sub {}; # see comment elsewhere
	    print LOG $log;
	} else {
	    print STDERR "WARNING: Failed to append to $logfile: $!\n";
	}
    }
    my $ret = $ua->post ($url, $params);
    print STDERR "server returned error\n", $ret->error_as_HTML, "\n"
	    unless $ret->is_success;
    if ($opened) {
	if ($ret->is_success) {
	    print LOG "changes sent to server\n";
	} else {
	    print LOG "server returned error\n", $ret->error_as_HTML, "\n";
	}
	close (LOG);
    }
}

sub got_match {
    my ($str, $pattern) = @_;

    return undef unless defined ($str) && $pattern;

    # If the pattern is delimited by slashes, run it directly ...
    if ($pattern =~ m,^/(.*)/([ix]*)$,) {
	eval "\$str =~ $pattern";
    } else {
	$str =~ $pattern;
    }
}

sub restore_echo_and_exit {
    system("stty echo");
    print "\n";
    exit(1);
}

sub prompt_password {
    my ($prompt) = @_;
    my $answer;
    my $echooff;

    # This might not work, since some versions of readline screw up
    # and turn on "echo" for us :-(

    $SIG{'INT'} = $SIG{'TERM'} = \&restore_echo_and_exit;
    system("stty -echo 2>/dev/null");
    if ($? == 0) {
	$echooff = 1;
    } else {
	$prompt .= "(will appear on screen): ";
    }
    $answer = prompt($prompt);
    if ($echooff) {
	print "\n";
	system("stty echo");
	$SIG{'INT'} = $SIG{'TERM'} = 'DEFAULT';
    }
    return $answer;
}
    
sub prompt {
    # $term is a global variable.  we initialise it here, so that it
    # is only done if the user actually needs prompting.
    $term = new Term::ReadLine 'listadmin'
	    unless $term;
    my $answer = $term->readline(@_);
    # readline turns off autoflush, re-enable it
    $| = 1;
    return $answer;
}

sub config_order {
    $config->{$a}{order} <=> $config->{$b}{order};
}
