#!/usr/bin/env -S perl -T
# SPDX-License-Identifier: BSD-3-Clause
# (c) 2020, Konstantin Demin

require 5.28.0;

use strict;
use strict 'vars';
use strict 'refs';
use warnings;

my $DEBUG = 0;

use Carp;       # using explicitly
use List::Util; # using explicitly
use POSIX;      # using explicitly
# use Text::Glob; # imported explicitly, using explicitly

use Getopt::Long qw( :config gnu_getopt );

#------------------------------------------------------#

# https://metacpan.org/pod/Text::Glob
# apt install libtext-glob-perl
package Text::Glob {
	use strict;

	use Exporter;
	use vars qw/ $VERSION @ISA @EXPORT_OK
	            $strict_leading_dot $strict_wildcard_slash
	            $seperator /;
	$VERSION = '0.11';
	@ISA = 'Exporter';
	@EXPORT_OK = qw( glob_to_regex_string );

	$strict_leading_dot    = 1;
	$strict_wildcard_slash = 1;
	$seperator             = undef;

	use constant debug => 0;

	sub glob_to_regex_string {
		my $glob = shift;

		my $seperator = $Text::Glob::seperator;
		$seperator = "/" unless defined $seperator;
		$seperator = quotemeta($seperator);

		my ($regex, $in_curlies, $escaping);
		local $_;
		my $first_byte = 1;
		for ($glob =~ m/(.)/gs) {
			if ($first_byte) {
				if ($strict_leading_dot) {
					$regex .= '(?=[^\.])' unless $_ eq '.';
				}
				$first_byte = 0;
			}
			if ($_ eq '/') {
				$first_byte = 1;
			}
			if ($_ eq '.' || $_ eq '(' || $_ eq ')' || $_ eq '|' ||
				$_ eq '+' || $_ eq '^' || $_ eq '$' || $_ eq '@' || $_ eq '%' ) {
				$regex .= "\\$_";
			}
			elsif ($_ eq '*') {
				$regex .= $escaping ? "\\*" :
				  $strict_wildcard_slash ? "(?:(?!$seperator).)*" : ".*";
			}
			elsif ($_ eq '?') {
				$regex .= $escaping ? "\\?" :
				  $strict_wildcard_slash ? "(?!$seperator)." : ".";
			}
			elsif ($_ eq '{') {
				$regex .= $escaping ? "\\{" : "(";
				++$in_curlies unless $escaping;
			}
			elsif ($_ eq '}' && $in_curlies) {
				$regex .= $escaping ? "}" : ")";
				--$in_curlies unless $escaping;
			}
			elsif ($_ eq ',' && $in_curlies) {
				$regex .= $escaping ? "," : "|";
			}
			elsif ($_ eq "\\") {
				if ($escaping) {
					$regex .= "\\\\";
					$escaping = 0;
				}
				else {
					$escaping = 1;
				}
				next;
			}
			else {
				$regex .= $_;
				$escaping = 0;
			}
			$escaping = 0;
		}
		print "# $glob $regex\n" if debug;

		return $regex;
	}
} ## Text::Glob

#------------------------------------------------------#

package Krd::Msg {
	use Carp;

	use feature 'signatures';
	no warnings 'experimental::signatures';

	sub warn ( $msg ) {
		Carp::cluck( "warning: $msg\n" );
		return undef;
	}

	sub die ( $msg ) {
		Carp::confess( "error: $msg\n" );
	}
} ## Krd::Msg

package Krd::Assert {
	use feature 'signatures';
	no warnings 'experimental::signatures';

	sub do ( $test, $msg ) {
		return if ( $test );
		Krd::Msg::die( $msg );
	}

	sub define (@) {
		foreach my $value ( @_ ) {
			Krd::Assert::do( defined( $value ),
				'value is undefined.'
			);
		}
	}

	sub argc ( $req_argc, $argv ) {
		Krd::Assert::define( $req_argc, $argv );

		my $argc = scalar @$argv;
		Krd::Assert::do( $argc >= $req_argc,
			"wrong number of arguments: expected $req_argc, got $argc."
		);
	}
} ## Krd::Assert

package Krd::Util {
	use List::Util;

	use feature 'signatures';
	no warnings 'experimental::signatures';

	sub ary_contains ( $str, @args ) {
		Krd::Assert::define( $str );

		my @ary = grep { defined } @args;
		return 0 if ( @ary == 0 );

		return ( List::Util::any { $_ eq $str } @ary ) ? 1 : 0;
		# foreach my $n ( @ary ) {
		# 	return 1 if ( $str eq $n );
		# }
		# return 0;
	}

	sub uniq ( @args ) {
		my @ary = grep { defined } @args;

		return List::Util::uniq( @ary );
		# my %seen;
		# return grep { !$seen{$_}++ } @ary;
	}
} ## Krd::Util

package Krd::Method {
	use feature 'signatures';
	no warnings 'experimental::signatures';

	sub is_with_instance ( $cname, $args ) {
		Krd::Assert::define( $args );

		return 0 if ( ! defined( $$args[0] ));

		my $t = ref $$args[0];
		return 0 if ( $t eq '' );

		if ( defined( $cname )) {
			Krd::Assert::do(
				$$args[0]->isa( $cname ),
				"method invoked on instance of $t, expected $cname."
			);
			return 1;
		}

		# theoretically unreachable path
		Krd::Assert::do(
			$$args[0]->isa( 'UNIVERSAL' ),
			"method invoked on instance of $t and something went wrong."
		);
	}

	sub skip_instance ( $cname, $args ) {
		Krd::Assert::define( $args );

		return if ( ! defined( $$args[0] ));

		my $t = ref $$args[0];
		return if ( $t eq '' );

		if ( defined( $cname )) {
			shift @$args if ( $$args[0]->isa( $cname ));
		}
	}

	sub is_with_class ( $cname, $args ) {
		Krd::Assert::define( $args );

		return 1 if ( ! defined( $$args[0] ));

		my $t = ref $$args[0];
		return 0 if ( $t ne '' );

		# krd: leave these comments for historical reason ;)
		# shift @$args if ( $$args[0] eq $cname );
		# shift @$args if ( $$args[0]->isa( $cname ) );
		return 1;
	}

	sub skip_class ( $cname, $args ) {
		Krd::Assert::define( $args );

		return if ( ! defined( $$args[0] ));

		my $t = ref $$args[0];
		return if ( $t ne '' );

		if ( defined( $cname )) {
			# shift @$args if ( $$args[0] eq $cname );
			shift @$args if ( $$args[0]->isa( $cname ));
		}
	}

	sub instance_only ( $cname, $args, $do_instance = undef ) {
		Krd::Assert::define( $args );

		Krd::Assert::do(
			Krd::Method::is_with_instance( $cname, $args ),
			'instance-only method.'
		);

		return &$do_instance( @$args )
		if ( defined( $do_instance ));
	}

	sub class_only ( $cname, $args, $do_class = undef ) {
		Krd::Assert::define( $args );

		Krd::Assert::do(
			Krd::Method::is_with_class( $cname, $args ),
			'class-only method.'
		);

		return &$do_class( @$args )
		if ( defined( $do_class ));
	}

	sub class_only_final ( $cname, $args, $do_class = undef ) {
		Krd::Assert::define( $args );

		Krd::Assert::do(
			Krd::Method::is_with_class( $cname, $args ),
			'class-only method.'
		);

		Krd::Method::skip_class( $cname, $args );

		return &$do_class( @$args )
		if ( defined( $do_class ));
	}

	sub hybrid ( $cname, $args, $do_class, $do_instance, $fallback = undef ) {
		Krd::Assert::define( $args );

		my $proc = $fallback;

		if ( Krd::Method::is_with_instance( $cname, $args )) {
			$proc = $do_instance;
		}
		elsif ( Krd::Method::is_with_class( $cname, $args )) {
			$proc = $do_class;
		}

		return &$proc( @$args )
		if ( defined( $proc ));
	}

	sub accessor ( $cname, $args, $ref, $default = undef, $verifier = undef ) {
		Krd::Assert::define( $args, $ref );

		Krd::Method::skip_instance( $cname, $args );
		Krd::Method::skip_class( $cname, $args );

		my $t = ref $ref;
		if ( $t eq 'ARRAY' ) {
			return @$ref if ( @$args == 0 );

			return ( @$ref = ( defined( $verifier ))
			               ? &$verifier( $ref, @$args )
			               : @$args );
		}
		elsif ( $t eq 'HASH' ) {
			return %$ref if ( @$args == 0 );

			return ( %$ref = ( defined( $verifier ))
			               ? &$verifier( $ref, @$args )
			               : @$args );
		}
		elsif ( $t eq 'SCALAR' ) {
			if ( @$args == 0 ) {
				$$ref = $default if ( ! defined( $$ref ));
				return $$ref;
			}

			return ( $$ref = ( defined( $verifier ))
			               ? &$verifier( $$ref, @$args )
			               : $$args[0] );
		}

		return undef;
	}

	sub xargs ( $args, $argc_common, $argc_proc, $proc, $underscore = 1 ) {
		Krd::Assert::define( $args, $argc_common, $argc_proc );

		$proc = sub { @_ } if ( ! defined( $proc ));

		$argc_common = 0 if ( $argc_common < 1 );

		my @argv_common;
		if ( $argc_common ) {
			Krd::Assert::argc( $argc_common, $args );
			@argv_common = splice( @$args, 0, $argc_common );
		}

		my ( @retary, $trval, $retref );
		$retref = \$trval;
		$retref = \$_ if ( $underscore and ( @$args == 0 ));
		$retref = \$_ if ( $underscore == 2 );

		$argc_proc = 0 if ( $argc_proc < 1 );
		$argc_proc = 0 if ( $argc_proc == @$args );

		if ( @$args == 0 ) {
			push @retary, &$proc( @argv_common, $_ );
		}
		elsif ( $argc_proc == 0 ) {
			push @retary, &$proc( @argv_common, @$args );
		}
		else {
			while ( my @i = splice( @$args, 0, $argc_proc )) {
				push @retary, &$proc( @argv_common, @i );
			}
		}

		return @retary if ( wantarray() );
		return ( $$retref = $retary[0] );
	}
} ## Krd::Method

package Krd::SmartValue {
	use feature 'signatures';
	no warnings 'experimental::signatures';

	# TODO: hide these methods
	sub R__value       ; # ( $bare, $flags, $arg )
	sub R__new         ; # ( $bare, $flags, @parts )
	sub R__eq          ; # ( $bare, $flags, $value, @parts )
	sub R__is_set      ; # ( $bare, $flags, $bits, $arg )
	sub R__get_own     ; # ( $mask, $arg )
	sub R__get_foreign ; # ( $mask, $arg )
	sub R__to_s        ; # ( $cprefix, $bare, $flags, $mask, $arg )

	# sub value ( $bare, $flags, @_ );
	sub value {
		return Krd::Method::xargs(
			\@_, 2, 1,
			\&R__value
		);
	}

	# sub new ( $bare, $flags, @_ );
	sub new {
		return Krd::Method::xargs(
			\@_, 2, 0,
			\&R__new
		);
	}

	# sub is_set ( $bare, $flags, $value, @_ );
	sub eq {
		return Krd::Method::xargs(
			\@_, 3, 0,
			\&R__eq
		);
	}

	# sub is_set ( $bare, $flags, $bits, @_ );
	sub is_set {
		return Krd::Method::xargs(
			\@_, 3, 1,
			\&R__is_set
		);
	}

	# sub get_own ( $mask, @_ );
	sub get_own {
		return Krd::Method::xargs(
			\@_, 1, 1,
			\&R__get_own
		);
	}

	# sub get_foreign ( $mask, @_ );
	sub get_foreign {
		return Krd::Method::xargs(
			\@_, 1, 1,
			\&R__get_foreign
		);
	}

	# sub to_s ( $cprefix, $bare, $flags, $mask, @_ );
	sub to_s {
		return Krd::Method::xargs(
			\@_, 4, 1,
			\&R__to_s
		);
	}

	sub reg {
		my ( $class, $a_bare, $r_bare, $a_flags, $r_flags, $r_mask ) = @_;
		Krd::Assert::define( $class );

		if ( defined( $a_bare ) && defined( $r_bare )) {
			foreach my $name ( @$a_bare ) {
				my $value = eval join( '::', $class, $name );
				$$r_bare{$value} = $name;
			}
		}

		if ( defined( $a_flags ) && defined( $r_flags )) {
			$$r_mask = 0;
			foreach my $name ( @$a_flags ) {
				my $value = eval join( '::', $class, $name );
				$$r_flags{$value} = $name;
				$$r_mask |= $value;
			}
		}
	}

	# non-public part

	sub R__value ( $bare, $flags, $arg ) {
		Krd::Assert::define( $bare, $flags, $arg );

		foreach my $k ( keys %{$bare} ) {
			return $k if ( $arg eq $k );
			return $k if ( $arg eq ${$bare}{$k} );
		}

		foreach my $k ( keys %{$flags} ) {
			return $k if ( $arg eq $k );
			return $k if ( $arg eq ${$flags}{$k} );
		}

		return $arg;
	}

	sub R__new ( $bare, $flags, @args ) {
		Krd::Assert::define( $bare, $flags );

		my $result = 0;

		my @parts = grep { defined } @args;

		foreach my $part ( @parts ) {
			$result += R__value( $bare, $flags, $part );
		}

		return $result;
	}

	sub R__eq ( $bare, $flags, $value, @parts ) {
		Krd::Assert::define( $bare, $flags, $value );

		my ( $a, $b );
		$a = R__value( $bare, $flags, $value );
		$b = R__new( $bare, $flags, @parts );

		return ( $a == $b ) ? 1 : 0;
	}

	sub R__is_set ( $bare, $flags, $bits, $arg ) {
		Krd::Assert::define( $bare, $flags, $bits, $arg );

		my ( $a, $b );
		$a = R__value( $bare, $flags, $arg );
		$b = R__value( $bare, $flags, $bits );

		return (( $a & $b ) == $b ) ? 1 : 0;
	}

	sub R__get_own ( $mask, $arg ) {
		Krd::Assert::define( $mask, $arg );

		return $arg & $mask;
	}

	sub R__get_foreign ( $mask, $arg ) {
		Krd::Assert::define( $mask, $arg );

		return $arg & ( ~( $mask ));
	}

	sub R__to_s ( $cprefix, $bare, $flags, $mask, $arg ) {
		Krd::Assert::define( $cprefix, $bare, $flags, $mask, $arg );

		my $t;

		foreach my $k ( keys %{$bare} ) {
			next if ( $arg != $k );

			$t = ${$bare}{$k};
			if ( $cprefix ) {
				$t = $cprefix . '::' . $t;
			}
			return $t;
		}

		if ( $mask == 0 ) {
			Krd::Msg::warn( "value is unknown ($arg)." );

			return $arg;
		}

		my @parts;

		foreach my $k ( keys %{$flags} ) {
			next if ( ! R__is_set( $bare, $flags, $k, $arg ));

			my $v = ${$flags}{$k};
			if ( $cprefix ) {
				$v = $cprefix . '::' . $v;
			}
			push @parts, $v;
		}

		my $foreign = R__get_foreign( $mask, $arg );
		if ( $foreign ) {
			# Krd::Msg::warn( "value ($arg) has 'foreign' bits ($foreign)." );

			push @parts, $foreign;
		}

		return join( ' | ', @parts );
	}
} ## Krd::SmartValue

#------------------------------------------------------#

package Krd::NameMangle {
	use constant CLASS_NAME => 'Krd::NameMangle';

	use List::Util;

	use feature 'signatures';
	no warnings 'experimental::signatures';

	our $strip_length = 1;

	my @params = qw/
		strip_length
	/;

	sub dump {
		my @result;

		push @result, sprintf( '%s:', CLASS_NAME );

		foreach my $p ( @params ) {
			my $v = eval ('$' . join( '::', CLASS_NAME, $p ));
			push @result, sprintf( '  %s = %s', $p, $v );
		}

		print join( "\n", @result ) . "\n";
	}

	sub strip_name_ex ( $name, $length = undef ) {
		Krd::Assert::define( $name );

		# /dev/null is special case
		return $name if ( $name eq '/dev/null' );

		my $r_len;

		if ( ! defined( $length )) {
			$r_len = $strip_length;
		}
		elsif ( $length =~ m/^([0-9]+)$/ ) {
			$r_len = $1;
		}
		elsif ( $length =~ m/^([+-])([0-9]+)$/ ) {
			$r_len = $strip_length;
			$r_len += $2 if ( $1 eq '+' );
			$r_len -= $2 if ( $1 eq '-' );
		}
		else {
			Krd::Msg::warn( "value is not recognized ('$length'), using $strip_length." );
			$r_len = $strip_length;
		}

		return $name if ( $r_len < 1 );

		my @parts = split( /\//, $name );

		return $name if ( @parts < 2 );

		my $d_len = List::Util::min( $r_len, @parts - 1 );
		splice( @parts, 0, $d_len );

		return join( '/', @parts );
	}

	sub strip_name {
		return Krd::Method::xargs(
			\@_, 0, 1,
			\&strip_name_ex
		);
	}

	sub reparse_names {
		my ( $i, $k, @a, @r );

		@a = grep { defined } @_;

		return @a if ( @a < 3 );

		$k = @a - 1;
		for ( $i = 0; $i < $k; $i++ ) {
			push @r, join( ' ', @a[      0 .. $i ] );
			push @r, join( ' ', @a[ $i + 1 .. $k ] );
		}

		return @r;
	}
} ## Krd::NameMangle

#------------------------------------------------------#

package Krd::NameSelectVerdict {
	use constant CLASS_NAME => 'Krd::NameSelectVerdict';

	use constant {
		KEEP   => 1,
		DELETE => 2,
	};
	my @values = qw/
		KEEP
		DELETE
	/;

	# API bridge to Krd::SmartValue

	my ( %_bare, %_flag );
	Krd::SmartValue::reg( CLASS_NAME,
		\@values, \%_bare
	);

	sub value ($) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::value( @_ );
	}

	sub new (@) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::new( @_ );
	}

	sub eq ($@) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::eq( @_ );
	}

	sub to_s ($) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, '', \%_bare, \%_flag, 0;
		return Krd::SmartValue::to_s( @_ );
	}
} ## Krd::NameSelectVerdict

package Krd::NameSelectKind {
	use constant CLASS_NAME => 'Krd::NameSelectKind';

	use constant {
		GLOB    => 1 << 0,
		REGEX   => 1 << 1,
		INCLUDE => 1 << 2,
		EXCLUDE => 1 << 3,
	};
	my @flags = qw/
		GLOB
		REGEX
		INCLUDE
		EXCLUDE
	/;

	use constant {
		NONE          => 0,
		INCLUDE_GLOB  => GLOB  + INCLUDE,
		INCLUDE_REGEX => REGEX + INCLUDE,
		EXCLUDE_GLOB  => GLOB  + EXCLUDE,
		EXCLUDE_REGEX => REGEX + EXCLUDE,
	};
	my @values = qw/
		NONE
		INCLUDE_GLOB
		INCLUDE_REGEX
		EXCLUDE_GLOB
		EXCLUDE_REGEX
	/;

	# API bridge to Krd::SmartValue

	my ( %_bare, %_flag, $_mask );
	Krd::SmartValue::reg( CLASS_NAME,
		\@values, \%_bare,
		\@flags,  \%_flag, \$_mask
	);

	sub value ($) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::value( @_ );
	}

	sub new (@) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::new( @_ );
	}

	sub eq ($@) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::eq( @_ );
	}

	sub is_set ($$) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::is_set( @_ );
	}

	sub to_s ($) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, '', \%_bare, \%_flag, $_mask;
		return Krd::SmartValue::to_s( @_ );
	}
} ## Krd::NameSelectKind

package Krd::NameSelectStrategy {
	use constant CLASS_NAME => 'Krd::NameSelectStrategy';

	use constant {
		SIMPLE => 0,
	};
	my @values = qw/
		SIMPLE
	/;

	# API bridge to Krd::SmartValue

	my ( %_bare, %_flag );
	Krd::SmartValue::reg( CLASS_NAME,
		\@values, \%_bare
	);

	sub value ($) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::value( @_ );
	}

	sub new (@) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::new( @_ );
	}

	sub eq ($@) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::eq( @_ );
	}

	sub to_s ($) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, '', \%_bare, \%_flag, 0;
		return Krd::SmartValue::to_s( @_ );
	}
} ## Krd::NameSelectStrategy

package Krd::NameSelector {
	use constant CLASS_NAME => 'Krd::NameSelector';

	# use Text::Glob;

	our $strategy = Krd::NameSelectStrategy::SIMPLE;

	my @selectors;

	# TODO: hide these methods
	sub C__new;
	sub C__verdict;
	sub I__verdict;
	sub R__verdict ($$);

	sub include_glob {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, 'INCLUDE_GLOB';
		C__new( @_ );
	}

	sub include_regex {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, 'INCLUDE_REGEX';
		C__new( @_ );
	}

	sub exclude_glob {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, 'EXCLUDE_GLOB';
		C__new( @_ );
	}

	sub exclude_regex {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, 'EXCLUDE_REGEX';
		C__new( @_ );
	}

	sub count {
		return scalar @selectors;
	}

	sub dump {
		my @result;

		push @result, sprintf( '%s:', CLASS_NAME );

		push @result, sprintf( '  %s = %s',
		                       'strategy',
		                       Krd::NameSelectStrategy::to_s( $strategy ));

		if ( @selectors > 0 ) {
			push @result, '  selectors:';
			foreach my $s ( @selectors ) {
				push @result, sprintf( '    kind = %s, arg = %s',
				                       Krd::NameSelectKind::to_s( $s->{kind} ),
				                       $s->{arg} );
			}
		}
		else {
			push @result, '  selectors: [empty]';
		}

		print join( "\n", @result ) . "\n";
	}

	sub verdict {
		return Krd::Method::hybrid(
			CLASS_NAME, \@_,
			\&C__verdict, \&I__verdict
		);
	}

	sub process_names {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );

		return map { $$_[0] }
		       grep { $$_[1] == Krd::NameSelectVerdict::KEEP }
		       verdict( @_ );
	}

	# non-public part

	sub C__new {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );

		my ( $kind, $arg ) = @_;
		Krd::Assert::define( $kind, $arg );

		$kind = Krd::NameSelectKind::new( $kind );

		my $self = bless {
			kind  => $kind,
			arg   => $arg,
			regex => undef,
		};

		if ( Krd::NameSelectKind::is_set( 'GLOB', $kind )) {
			my $r = Text::Glob::glob_to_regex_string($arg);
			# $self->{regex} = qr/^$r$/;
			$self->{regex} = qr/$r/;
		}

		if ( Krd::NameSelectKind::is_set( 'REGEX', $kind )) {
			# $self->{regex} = qr/^$arg$/;
			$self->{regex} = qr/$arg/;
		}

		push @selectors, $self;
		return $self;
	}

	sub R__verdict ($$) {
		my ( $self, $arg ) = @_;
		Krd::Assert::define( $self, $arg );

		my $match = ( $arg =~ $self->{regex} ) ? 1 : 0;
		my $include = Krd::NameSelectKind::is_set( 'INCLUDE', $self->{kind} );

		return ( $match == $include )
		       ? Krd::NameSelectVerdict::KEEP
		       : Krd::NameSelectVerdict::DELETE;
	}

	sub I__verdict {
		return Krd::Method::xargs(
			\@_, 1, 1,
			\&R__verdict
		);
	}

	sub C__verdict {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );

		my ( @names, @result, @new, @curr );

		@names = grep { defined } @_;

		if ( $strategy == Krd::NameSelectStrategy::SIMPLE ) {
			foreach my $sel ( @selectors ) {
				@new = ( $sel->verdict( @names ));

				for ( my $i = 0; $i < @new; $i++ ) {
					if ( ! defined( $curr[$i] )) {
						$curr[$i] = $new[$i];
						next;
					}

					next if ( $curr[$i] == Krd::NameSelectVerdict::DELETE );

					$curr[$i] = $new[$i];
				}
			}
		}

		for ( my $i = 0; $i < @curr; $i++ ) {
			$result[ $i ] = [ $names[$i], $curr[$i] || Krd::NameSelectVerdict::KEEP ];
		}

		return @result;
	};
} ## Krd::NameSelector

#------------------------------------------------------#

package Krd::DiffEntryType {
	use constant CLASS_NAME => 'Krd::DiffEntryType';

	use constant {
		NONE => 0,
	};
	my @values = qw/
		NONE
	/;

	use constant {
		COMMENT => 1 << 0,
		DIFF    => 1 << 1,
		NEW     => 1 << 2,
		DELETE  => 1 << 3,
		RENAME  => 1 << 4,
	};
	my @flags = qw/
		COMMENT
		DIFF
		NEW
		DELETE
		RENAME
	/;

	# API bridge to Krd::SmartValue

	my ( %_bare, %_flag, $_mask );
	Krd::SmartValue::reg( CLASS_NAME,
		\@values, \%_bare,
		\@flags,  \%_flag, \$_mask
	);

	sub value ($) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::value( @_ );
	}

	sub new (@) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::new( @_ );
	}

	sub eq ($@) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::eq( @_ );
	}

	sub is_set ($$) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, \%_bare, \%_flag;
		return Krd::SmartValue::is_set( @_ );
	}

	sub to_s ($) {
		Krd::Method::class_only_final( CLASS_NAME, \@_ );
		unshift @_, '', \%_bare, \%_flag, $_mask;
		return Krd::SmartValue::to_s( @_ );
	}
} ## Krd::DiffEntryType

package Krd::DiffEntry {
	use constant CLASS_NAME => 'Krd::DiffEntry';

	sub init ($@) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self, @args ) = @_;

		my $line = shift @args;

		$self->{line} = $line;
		$self->{length} = 1;
		$self->type_set( 0, @args );
	}

	sub new ($@) {
		Krd::Method::class_only( CLASS_NAME, \@_ );

		my ( $class, @args ) = @_;

		my $self = bless { }, $class;

		$self->init( @args );

		return $self;
	}

	sub line ($) : lvalue {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		return $_[0]->{line};
	}

	sub length ($) : lvalue {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		return $_[0]->{length};
	}

	sub type ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		return $_[0]->{type};
	}

	sub type_set ($@) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self, @args ) = @_;

		$self->{type} = Krd::DiffEntryType::new( @args );
	}

	sub type_is ($@) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self, @args ) = @_;

		return Krd::DiffEntryType::eq( $self->{type}, @args );
	}

	sub type_has ($@) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self, @args ) = @_;

		return Krd::DiffEntryType::is_set(
			Krd::DiffEntryType::new( @args ),
			$self->{type}
		);
	}

	sub type_add ($@) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self, @args ) = @_;

		$self->{type} |= Krd::DiffEntryType::new( @args );
	}

	sub copy_to ($$) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self, $new ) = @_;

		foreach my $f ( qw/ line length type / ) {
			$new->{$f} = $self->{$f};
		}

		return $new;
	}

	sub base_dup ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self ) = @_;

		my $new = Krd::DiffEntry->new( $self->{line} );
		return $self->copy_to( $new );
	}

	sub dup ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self ) = @_;

		my $new = ( ref $self )->new( $self->{line} );
		return $self->copy_to( $new );
	}

	sub to_s ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self ) = @_;

		my @result;

		push @result, sprintf( '%s:', $self );

		foreach my $f ( qw/ line length / ) {
			push @result, sprintf( '  %s: %s', $f, $self->{$f} );
		}

		push @result, sprintf( '  type: %s',
		                       Krd::DiffEntryType::to_s( $self->{type} ));

		return join( "\n", @result );
	}
} ## Krd::DiffEntry

package Krd::DiffEntry::Comment {
	use parent -norequire, 'Krd::DiffEntry';

	use constant CLASS_NAME => 'Krd::DiffEntry::Comment';

	sub new ($@) {
		Krd::Method::class_only( CLASS_NAME, \@_ );

		my ( $class, @args ) = @_;

		my $self = bless { }, $class;

		push @args, 'COMMENT';
		$self->init( @args );

		return $self;
	}
} ## Krd::DiffEntry::Comment

package Krd::DiffEntry::Diff {
	use parent -norequire, 'Krd::DiffEntry';

	use constant CLASS_NAME => 'Krd::DiffEntry::Diff';

	sub init ($@) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );

		my ( $self, @args ) = @_;

		$self->SUPER::init( @args );

		$self->{names} = [];
	}

	sub new ($@) {
		Krd::Method::class_only( CLASS_NAME, \@_ );

		my ( $class, @args ) = @_;

		my $self = bless { }, $class;

		push @args, 'DIFF';
		$self->init( @args );

		return $self;
	}

	sub names ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		return @{$_[0]->{names}};
	}

	sub names_count ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		return scalar @{$_[0]->{names}};
	}

	sub names_has ($$) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self, $arg ) = @_;
		return Krd::Util::ary_contains( $arg, @{$self->{names}} );
	}

	sub names_clear ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		$_[0]->{names} = [];
	}

	sub name_add ($@) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self, @args ) = @_;

		foreach my $arg ( @args ) {
			next if $self->names_has( $arg );
			push @{$self->{names}}, $arg;
		}
	}

	sub copy_to ($$) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self, $new ) = @_;

		$self->SUPER::copy_to( $new );

		if ( $new->isa( CLASS_NAME )) {
			# only copy to same class
			$new->{names} = [ @{$self->{names}} ];
		}

		return $new;
	}

	sub dup ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self ) = @_;

		my $new = $self->SUPER::dup();

		return $self->copy_to( $new );
	}

	sub to_s ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self ) = @_;

		my $base = $self->SUPER::to_s();
		my @result;

		if ( $self->names_count() > 0 ) {
			push @result, '  names:';
			foreach my $n ( @{$self->{names}} ) {
				push @result, '    ' . $n;
			}
		}
		else {
			push @result, '  names: [empty]';
		}

		return join( "\n", $base, @result );
	}
} ## Krd::DiffEntry::Diff

package Krd::DiffFile {
	use List::Util;
	use POSIX;

	use constant CLASS_NAME => 'Krd::DiffFile';

	our $do_inplace        = 0;

	our $strip_comments    = 0;
	our $strip_new         = 0;
	our $strip_deleted     = 0;
	our $strip_renames     = 0;
	our $strip_regular     = 0;

	my @params = qw/
		do_inplace
	/;

	my @selectors = qw/
		strip_comments
		strip_new
		strip_deleted
		strip_renames
		strip_regular
	/;

	# TODO: hide these methods
	sub C__dump;
	sub I__dump ($);
	sub _gets ($);
	sub _gets_supress ($);
	sub _ent_store ($);
	sub _ent_none ($);
	sub _ent_comment;
	sub _ent_diff ($);
	sub _hdr_ext_process ($);
	sub _hdr_uni_process ($);
	sub _hdr_uni_strip_timestamp;
	sub _hunk_walker ($);

	sub new {
		Krd::Method::class_only( CLASS_NAME, \@_ );
		my ( $class, $file ) = @_;
		Krd::Assert::define( $class, $file );

		# untaint
		if ( $file =~ m/^(.+)$/ ) { $file = $1; }

		my $self = bless {
			file => $file,
		}, $class;

		$self->{entries} = [ ];

		return $self;
	}

	sub dump {
		return Krd::Method::hybrid(
		    CLASS_NAME, \@_,
		    \&C__dump, \&I__dump
		);
	}

	sub selector_count {
		# return ( $strip_comments
		#        + $strip_new
		#        + $strip_deleted
		#        + $strip_renames
		#        + $strip_regular
		# );

		my $r = 0;

		foreach my $p ( @selectors ) {
			my $v = eval ('$' . join( '::', CLASS_NAME, $p ));
			$r += $v;
		}

		return $r;
	}

	sub selector_max {
		return scalar @selectors;
	}

	sub process_file ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self ) = @_;

		if ( @{$self->{entries}} ) {
			return Krd::Msg::warn( "$self->{file}: already read." );
		}

		open( $self->{handle}, '<', $self->{file} )
		or return Krd::Msg::warn( "$self->{file}: cannot open." );

		$self->{line}    = 0;
		$self->{entries} = [ ];
		# $self->{ent} is initialized inside
		$self->_ent_none();

		while ( $self->_gets() ) {
			if ( m/^(diff|---) / ) {
				if ( $self->{ent}->type_is( 'COMMENT' )) {
					$self->_ent_store();
				}

				if ( ! $self->{ent}->type_has( 'DIFF' )) {
					$self->_ent_diff();
				}

				if ( $1 eq '---' ) {
					$self->_hdr_uni_process();
				}
				else {
					$self->_hdr_ext_process();
				}

				next;
			}

			if ( $self->{ent}->type_has( 'DIFF' )) {
				$self->_ent_store();
			}

			if ( ! $self->{ent}->type_is( 'COMMENT' )) {
				$self->_ent_comment();
			}
		}

		delete $self->{ent};
		delete $self->{line};
		# $self->{eof} initialized inside _gets()
		delete $self->{eof};

		close( $self->{handle} )
		or Krd::Msg::warn( "$self->{file}: close failed with $!." );
		delete $self->{handle};
	}

	sub grep_away ($\[$&]) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self, $proc ) = @_;
		Krd::Assert::define( $self, $proc );

		@{$self->{entries}} = grep { ! &$proc( $_ ) }
		                      @{$self->{entries}};
	}

	sub apply_filters ($$) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self, $verdict_sub ) = @_;
		Krd::Assert::define( $self, $verdict_sub );

		if ( @{$self->{entries}} == 0 ) {
			return warn( "$self->{file}: has no entries, nothing to do.\n" );
		}

		if ( $strip_comments ) {
			$self->grep_away( sub { $_->type_is( 'COMMENT' ) } );
		}

		if ( $strip_new ) {
			$self->grep_away( sub { $_->type_has( 'NEW' ) } );
		}

		if ( $strip_deleted ) {
			$self->grep_away( sub { $_->type_has( 'DELETE' ) } );
		}

		if ( $strip_renames ) {
			$self->grep_away( sub { $_->type_has( 'RENAME' ) } );
		}

		if ( $strip_regular ) {
			$self->grep_away( sub { $_->type_is( 'DIFF' ) } );
		}

		$self->grep_away( sub {
			# skip comments:
			# they don't know anything about file names
			return 0 if ( ! $_->type_has( 'DIFF' ));

			return List::Util::any { Krd::NameSelectVerdict::eq( 'DELETE' ) }
			       map { $$_[1] }
			       &$verdict_sub( $_->names() );
		} );
	}

	sub write_changes ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self ) = @_;

		if ( @{$self->{entries}} == 0 ) {
			return warn( "$self->{file}: has no entries left, nothing to do.\n" );
		}

		$self->{line} = 0;

		open( $self->{handle}, '<', $self->{file} )
		or return Krd::Msg::warn( "$self->{file}: cannot open." );

		my ( $s, $t );

		$s = $self->{file} . '.new.' . POSIX::strftime( '%Y%m%d-%H%M%S', localtime());
		open( $t, '>', $s )
		or return Krd::Msg::warn( "$s: cannot open." );

		foreach my $e ( @{$self->{entries}} ) {
			my ( $a, $b, $i );
			( $a, $b ) = ( $e->line, $e->length );

			while ( $self->{line} < $a ) {
				$self->_gets();
			}

			for ( $i = 0 ; $i < $b ; $i++ ) {
				printf $t "%s\n", $_;
				$self->_gets();
			}
		}

		close( $t )
		or Krd::Msg::warn( "$s: close failed with $!." );

		delete $self->{line};
		# $self->{eof} initialized inside _gets()
		delete $self->{eof};

		close( $self->{handle} )
		or Krd::Msg::warn( "$self->{file}: close failed with $!." );
		delete $self->{handle};

		if ( $do_inplace ) {
			unlink( $self->{file} );
			rename( $s, $self->{file} );
		}
	}

	sub close_file ($) {
		Krd::Method::instance_only( CLASS_NAME, \@_ );
		my ( $self ) = @_;

		delete $self->{entries};
	}

	# non-public part

	sub C__dump {
		my @result;

		push @result, sprintf( '%s:', CLASS_NAME );

		foreach my $p ( @params ) {
			my $v = eval ('$' . join( '::', CLASS_NAME, $p ));
			push @result, sprintf( '  %s = %s', $p, $v );
		}
		foreach my $p ( @selectors ) {
			my $v = eval ('$' . join( '::', CLASS_NAME, $p ));
			push @result, sprintf( '  %s = %s', $p, $v );
		}

		print join( "\n", @result ) . "\n";
	}

	sub I__dump ($) {
		my ( $self ) = @_;

		my @result;

		push @result, sprintf( '%s:', $self );
		push @result, sprintf( '  file: %s', $self->{file} );

		if ( @{$self->{entries}} > 0 ) {
			push @result, '  entries:';

			my ( $e, @t );

			foreach $e ( @{$self->{entries}} ) {
				@t = split( "\n", $e->to_s() );

				# re-indent strings
				@t = map { $_ = '    ' . $_ } @t;

				push @result, @t;
			}
		}
		else {
			push @result, '  entries: [empty]';
		}

		print join( "\n", @result ) . "\n";
	}

	sub _gets ($) {
		my ( $self ) = @_;

		if ( $self->{gets_supress} ) {
			delete $self->{gets_supress};
			return 1;
		}

		$_ = undef;

		return if ( ! defined( $self->{handle} ));
		return if ( $self->{eof} );

		$self->{eof} = 1 if ( eof( $self->{handle} ));

		$_ = readline( $self->{handle} );
		$self->{line}++;

		if ( ! defined( $_ )) {
			return if ( ! $self->{eof} );

			$_ = '';
		}

		chomp;

		return 1;
	}

	sub _gets_supress ($) {
		my ( $self ) = @_;
		$self->{gets_supress} = 1;
	}

	sub _ent_store ($) {
		my ( $self ) = @_;

		my $t = $self->{ent}->dup();
		$t->length = $self->{line} - $t->line();

		push @{$self->{entries}}, $t;
	}

	sub _ent_none ($) {
		my ( $self ) = @_;
		$self->{ent} = Krd::DiffEntry->new( $self->{line} );
	}

	sub _ent_comment {
		my ( $self, $convert ) = @_;

		my $e = Krd::DiffEntry::Comment->new( $self->{line} );
		if ( $convert ) {
			$self->{ent}->copy_to( $e );
			$e->type_set( 'COMMENT' );
		}
		$self->{ent} = $e;
	}

	sub _ent_diff ($) {
		my ( $self ) = @_;
		$self->{ent} = Krd::DiffEntry::Diff->new( $self->{line} );
	}

	sub _hdr_ext_process ($) {
		my ( $self ) = @_;

		s/^diff --git //;
		s/^diff //;

		my ( @guess, $n );
		@guess = split( / /, $_ );

		while ( $self->_gets() ) {
			next if ( m/^index / );

			if ( m/^old mode / ) {
				goto hdr_ext_fail if ( ! $self->_gets());
				goto hdr_ext_fail if ( ! m/^new mode / );

				next;
			}

			if ( m/^(deleted|new) file mode / ) {
				$self->{ent}->type_add(( $1 eq 'new' ) ? 'NEW' : 'DELETE' );

				next;
			}

			if ( m/^similarity index / ) {
				# not needed anymore
				@guess = [ ];

				$self->{ent}->type_add( 'RENAME' );

				goto hdr_ext_fail if ( ! $self->_gets());
				goto hdr_ext_fail if ( ! m/^rename from (.+)$/ );

				# $n = $1;
				$n = Krd::NameMangle::strip_name_ex( $1, '-1' );
				$self->{ent}->name_add( $n );

				goto hdr_ext_fail if ( ! $self->_gets());
				goto hdr_ext_fail if ( ! m/^rename to (.+)$/ );

				# $n = $1;
				$n = Krd::NameMangle::strip_name_ex( $1, '-1' );
				$self->{ent}->name_add( $n );

				next;
			}

			if ( m/^(---|diff) / ) {
				if ( $self->{ent}->names_count() == 0 ) {
					@guess = map { Krd::NameMangle::strip_name }
					         Krd::NameMangle::reparse_names( @guess );

					$self->{ent}->name_add( @guess );
				}

				if ( $1 eq 'diff' ) {
					$self->_ent_store();
					$self->_ent_diff();
				}

				$self->_gets_supress();
				return; # back to process_file()
			}

			if ( m/^$/ ) {
				return if ( $self->{ent}->names_count() > 0 );
			}

			goto hdr_ext_fail;
		}

		return;

		hdr_ext_fail:

		# (force) convert to comment
		$self->_ent_comment( 1 );
	}

	sub _hdr_uni_strip_timestamp {
		# strip possible timestamp
		# operates on $_
		s/\t\d{2}:\d{2}:\d{2}(|\.\d+)(| [+-]\d+)$//n;
	}

	sub _hdr_uni_process ($) {
		my ( $self ) = @_;

		my ( @guess, $n );

		my ( $ok, $is_new, $a, $b );
		$ok = $is_new = $a = $b = 0;

		s/^--- //;

		_hdr_uni_strip_timestamp();

		# $_ = Krd::NameMangle::strip_name( $_ );
		Krd::NameMangle::strip_name;

		push @guess, $_;

		$a = $self->{ent}->names_has( $_ );

		goto hdr_uni_fail if ( ! $self->_gets());

		if ( m/^\+{3} (.+)$/ ) {
			$_ = $1;

			_hdr_uni_strip_timestamp();

			# $_ = Krd::NameMangle::strip_name( $_ );
			Krd::NameMangle::strip_name;

			push @guess, $_;

			$b = $self->{ent}->names_has( $_ );

			$ok = 1;
		}

		if ( $ok ) {
			if ( $self->{ent}->type_has( 'NEW' )) {
				$is_new = $b ? 0 : 1;
			}
			elsif ( $self->{ent}->type_has( 'DELETE' )) {
				$is_new = $a ? 0 : 1;
			}
			elsif ( $self->{ent}->names_count() > 0 ) {
				$is_new = ( $a and $b ) ? 0 : 1;
			}
		} else {
			goto hdr_uni_fail if ( $a );

			if ( $self->{ent}->type_has( 'NEW' )) {
				$is_new = ( $guess[0] ne '/dev/null' ) ? 1 : 0;
			}
			else {
				$is_new = ( $guess[0] eq '/dev/null' ) ? 1 : 0;
			}
		}

		if ( $is_new ) {
			$self->_ent_store();
			$self->_ent_diff();
			$self->{ent}->line()--;
		}

		if ( ! $ok ) {
			goto hdr_uni_fail;
		}

		if ( $guess[0] eq '/dev/null' ) {
			$self->{ent}->type_add( 'NEW' );
			shift @guess;
		}
		elsif ( $guess[1] eq '/dev/null' ) {
			$self->{ent}->type_add( 'DELETE' );
			pop @guess;
		}
		elsif ( $guess[0] ne $guess[1] ) {
			$self->{ent}->type_add( 'RENAME' );
		}

		$self->{ent}->names_clear();
		$self->{ent}->name_add( @guess );

		return $self->_hunk_walker();

		hdr_uni_fail:

		$self->_gets_supress();
		# (force) convert to comment
		$self->_ent_comment( 1 );
	}

	my $_hunk_hdr = qr/^@@ -(?:\d+)(?:|,(?<x>\d+)) \+(?:\d+)(?:|,(?<y>\d+)) @@.*$/;
	sub _hunk_walker ($) {
		my ( $self ) = @_;

		my $c_hunk = 0;
		my ( $x, $y, $i );
		my ( $z0, $z1, $zm );
		my ( $a1, $b1, $c1 );

		while ( 1 ) {
			goto hunk_walker_fail if ( ! $self->_gets() );
			last if ( ! m/$_hunk_hdr/ );

			# $x = lines from original file
			# $y = lines from new file
			#
			# $x = $a0 + $c0
			# $y = $b0 + $c0
			#
			#   $a0 = deleted lines
			#   $b0 = added lines
			#   $c0 = unchanged lines
			#
			# $z0 = overall hunk content length (except header)
			# $z0 = $a0 + $b0 + $c0
			#
			# $z1 = safe to `slurp` hunk content lines
			# $z1 = max($x, $y)
			#
			# $zm = safety counter (actually beyond hunk length!)
			# $zm = $x + $y
			#
			# brief:
			# parse at least $z1 lines:
			#     count $a1, $b1 and $c1 while parsing.
			# parse line by line:
			#     compare $a1 + $c1 with $x and $b1 + $c1 with $y.
			#     when comparison will succeed:
			#         calculate $a0, $b0 and $c0;
			#         parse remaining lines.

			$x = defined( $x = $+{x} ) ? $x + 0 : 1;
			$y = defined( $y = $+{y} ) ? $y + 0 : 1;

			$z0 = undef;
			$z1 = List::Util::max( $x, $y );
			$zm = $x + $y;

			# my ( $a0, $b0, $c0 );
			$a1 = $b1 = $c1 = 0;

			# corner cases
			if ( $x == 0 ) { $z0 = $zm = $y; }
			if ( $y == 0 ) { $z0 = $zm = $x; }

			for ( $i = 0 ; $i < $zm; $i++ ) {
				if ( $i >= $z1 ) {
					if (( $a1 + $c1 ) == $x ) {
						# $a0 = $a1;
						# $b0 = $y - $c1;
						# $c0 = $c1;
						$z0 = $a1 + $y;
					}

					if (( $b1 + $c1 ) == $y ) {
						# $a0 = $x - $c1;
						# $b0 = $b1;
						# $c0 = $c1;
						$z0 = $b1 + $x;
					}
				}

				if ( defined( $z0 )) {
					last if ( $i == $z0 );
				}

				goto hunk_walker_fail if ( ! $self->_gets() );
				goto hunk_walker_fail if ( ! m/^(.)/ );

				if    ( $1 eq '-' ) { $a1++; next; }
				elsif ( $1 eq '+' ) { $b1++; next; }
				elsif ( $1 eq ' ' ) { $c1++; next; }

				goto hunk_walker_fail;
			}

			goto hunk_walker_fail if ( ! defined( $z0 ));
			goto hunk_walker_fail if ( $i != $z0 );

			$c_hunk++;
		}

		goto hunk_walker_fail if ( $c_hunk == 0 );

		$self->_gets_supress();

		$self->_ent_store();
		$self->_ent_none();

		return;

		hunk_walker_fail:

		$self->_gets_supress();
		# (force) convert to comment
		$self->_ent_comment( 1 );
	}
} ## Krd::DiffFile

#------------------------------------------------------#

Getopt::Long::GetOptions(
	'd|debug'            => sub { $DEBUG = 1; },

	'i|include=s@'       => sub { Krd::NameSelector::include_glob(  $_[1] ); },
	'I|include-regex=s@' => sub { Krd::NameSelector::include_regex( $_[1] ); },
	'x|exclude=s@'       => sub { Krd::NameSelector::exclude_glob(  $_[1] ); },
	'X|exclude-regex=s@' => sub { Krd::NameSelector::exclude_regex( $_[1] ); },

	's|strip:i'          => sub { die "negative value in '$_[0]'.\n" if ( $_[1] < 0);
	                              $Krd::NameMangle::strip_length = $_[1]; },
	'n|no-strip|nostrip' => sub { $Krd::NameMangle::strip_length = 0; },

	'Z|inplace|in-place' => sub { $Krd::DiffFile::do_inplace = 1; },

	'C|strip-comments'   => sub { $Krd::DiffFile::strip_comments = 1; },
	'N|strip-new'        => sub { $Krd::DiffFile::strip_new      = 1; },
	'D|strip-deleted'    => sub { $Krd::DiffFile::strip_deleted  = 1; },
	'R|strip-renames'    => sub { $Krd::DiffFile::strip_renames  = 1; },
	'U|strip-regular'    => sub { $Krd::DiffFile::strip_regular  = 1; },
) or die "error in parsing options.\n";

if ( $DEBUG ) {
	Krd::NameSelector::dump();
	Krd::DiffFile::dump();
	Krd::NameMangle::dump();
}

if ( @ARGV == 0 ) {
	die "no files are specified.\n";
}

if (( Krd::NameSelector::count() + Krd::DiffFile::selector_count() ) == 0 ) {
	die "no selectors are specified.\n";
}

if ( Krd::DiffFile::selector_count() == Krd::DiffFile::selector_max() ) {
	die "specified selectors are throwing away whole file(s).\n";
}

foreach my $f ( @ARGV ) {
	if ( ! -e $f ) {
		warn "file '$f' does not exist.\n";
		next;
	}
	if ( ! -r $f ) {
		warn "file '$f' is not accessible.\n";
		next;
	}
	if ( ! -w $f ) {
		warn "file '$f' is not writable.\n";
		next;
	}

	my $d = Krd::DiffFile->new( $f );

	$d->process_file();

	$d->dump() if ( $DEBUG );

	$d->apply_filters( \&Krd::NameSelector::verdict );

	$d->dump() if ( $DEBUG );

	$d->write_changes();
	$d->close_file();
}
