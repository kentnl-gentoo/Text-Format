package Text::Format;

=head1 NAME

Text::Format - Various subroutines to manipulate text.

=head1 SYNOPSIS

    use Text::Format;

    $text = Text::Format->new({
        columns        => 72,
        tabstop        =>  8,
        firstIndent    => "\t",
        bodyIndent     => '',
        rightFill      => 0,
        rightAlign     => 0,
        leftMargin     => 0,
        rightMargin    => 0,
        expandTabs     => 0,
        extraSpace     => 0,
        abbrevs        => {}, # reference to a hash
        text           => [], # reference to a list
        hangingIndent  => 0,
        hangingText    => [], # reference to a list
    }); # these are the default values

    $text = Text::Format->new();
    print $text->wrap(@text);
    print $text->fill(@text);
    print $text->center(@text);
    print $text->wrap([<FILEHANDLE>]);
    print $text->fill([<FILEHANDLE>]);
    print $text->expand(@text);
    print $text->unexpand(@text);

    $text = Text::Format->new
        ({tabstop => 4,bodyIndent => "\t",text => \@text});
    print $text->wrap();
    print $text->fill();
    print $text->center();
    print $text->expand();
    print $text->unexpand();

    print Text::Format->new->wrap(@text);
    %abbr = (foo => 1, bar => 1);
    $text->abbrevs(\%abbr);
    $text->abbrevs();
    $text->abbrevs(qw/foo bar/);
    $text->text(\@text);

    $text->columns(132);
    $text->tabstop(4);
    $text->expandTabs(1);
    $text->extraSpace(1);
    $text->firstIndent("\t\t");
    $text->bodyIndent("\t"
    $text->config({tabstop => 4,firstIndent => ''});
    $text->rightFill(0);
    $text->rightAlign(0);

=head1 DESCRIPTION

The wrap routine will wrap under all circumstances even if the width
isn't enough to contain the longest words.  Text::Wrap will die under
these circumstances which isn't quite desirable in my opinion.  If
columns is set to a small number and words are longer than that and the
leading 'whitespace' than there will be a single word on each line.
This will let you make a simple word list which could be indented or
right aligned.  There is a chance for croaking if you try to subvert the
module.
General setup should be explained with the below graph.

                              Columns
<------------------------------------------------------------>
<---------><----->                                <---------->
Left Margin Indent                                Right Margin

=over 4

=item wrap @ARRAY || \@ARRAY || [<FILEHANDLE>] || NOTHING

Allows to do basic formatting of text into paragraphs, with indent for
first line and following lines separately.  Can specify tab size and
columns, width of total text, right fill with spaces and right align,
right margin and left margin.  Strips all leading and trailing
whitespace before proceding.  If right alignment is set or tab expansion
is set or hanging indents is set then all tabs are expanded to spaces.

=item fill @ARRAY || \@ARRAY || [<FILEHANDLE>] || NOTHING

Considers each element of text as a paragraph and if the indents are the
same for first line and the rest of the lines then they are separated by
a single empty line otherwise they follow one under the other.  If
hanging indent is set then a single empty line will separate each
paragraph as well.  Calls wrap to do the actual formatting.

=item center @ARRAY || NOTHING

Centers a list of strings in @ARRAY or internal text.  Empty lines
appear as, you guessed it, empty lines.  Center strips all leading and
trailing whitespace before proceding.  Left margin and right margin can
be set.

=item expand @ARRAY || NOTHING

Expand tabs in the list of text to tabstop number of spaces in @ARRAY or
internal text.

=item unexpand @ARRAY || NOTHING

Tabstop number of spaces are turned into tabs in @ARRAY or internal
text.

=item columns NUMBER || NOTHING

Set width of text or retrieve width.  This is total width and includes
indentation and the right and left margins.

=item tabstop NUMBER || NOTHING

Set tabstop size or retrieve tabstop size.

=item rightFill 0 || 1 || NOTHING

Set right fill to true or retrieve its value.

=item rightAlign 0 || 1 || NOTHING

Set right align to true or retrieve its value.

=item leftMargin NUMBER || NOTHING

Set or get width of left margin.

=item rightMargin NUMBER || NOTHING

Set or get width of right margin.

=item expandTabs 0 || 1 || NOTHING

Expand leading tabs to spaces, or in case of center, expand internal
tabs.  Returns current setting of attribute.

=item abbrevs \%HASH || @ARRAY || NOTHING

Add to the current abbreviations, takes a reference to your array, if
called a second time the original reference is removed.  Returns the
current INTERNAL abbreviations.

=item extraSpace 0 || 1 || NOTHING

Add extra space after end of sentence, normally wrap would add 1 space
after end of sentence, if this is set to 1 then 2 spaces are used.

=item hangingText \@ARRAY || NOTHING

The text that will be displayed in front of each paragraph, if you call
wrap than only the first element is used, if you call fill then fill
cycles through all of them.  If you have more paragraphs than elements in
your array than the first one will get reused.  Pass a reference to your
array.

=item hangingIndent 0 || 1 || NOTHING

Use hanging indents in front of a paragraph, returns current value of
attribute.

=item config \%HASH

Allows the configuration of all object attributes at once.

=item text \@ARRAY || NOTHING

Pass in a reference to your text that you want the routines to
manipulate.  Returns the text held in the object.

=back

=head1 EXAMPLE

    use Text::Format;

    $text = new Text::Format;
    $text->rightFill(1);
    $text->columns(65);
    $text->tabstop(4);
    print $text->wrap("a line to format to an indented regular
            paragraph using 65 character wide display and a
            tabstop of 4");
    $text->expandTabs(1); # tab will be expanded to spaces
    print $text->fill("paragraph one","paragraph two");
    print $text->center("hello world","nifty line 2");
    print $text->expand("\t\thello world\n","hmm,\twell\n");
    print $text->unexpand("    hello world\n","    hmm");
    $text->config({columns => 132, tabstop => 4});

=head1 BUGS

Line length can exceed columns specified if columns is set to a small
number and long words plus leading whitespace exceed column length
specified.  Actually I see this as a feature since it can be used to
make up a nice wordlist.

=head1 AUTHOR

Gabor Egressy <gabor@vmunix.com>, some suggestions for improvement by
Tom Phoenix, Brad Appleton, Byron Brummer, and Andreas Koenig

Copyright (c) 1998 Gabor Egressy.  All rights reserved.  All wrongs
reversed.  This program is free software; you can redistribute and/or
modify it under the same terms as Perl itself.

=head1 TODO

    Add the following features :

    1.  support for non-breaking whitespace
        - use hash to store the regex on which not to break eg.
          %hash = ('Mrs?\.' => '^\S+$');

=cut

use Carp;
use strict;
use vars qw($VERSION);

$VERSION = '0.33';

my ($make_line,%abbrev,$is_abbrev);

# local abbreviations, you can add your own with add_abbrevs()
%abbrev = (Mr  => 1,
           Mrs => 1,
           Ms  => 1,
           Sr  => 1,
           Jr  => 1,
);

# similar to Text::Wrap::wrap except that it sticks a newline at the end
# of the last line and if so configured puts two spaces after a
# (period|question mark|exclamation mark) if it ends a word unless that
# word is in the abbreviations hash; it also doesn't die when columns is
# shorter than the longest word + leading whitespace on the first line;
# will also do right alignment or right fill with spaces
sub wrap(\$@)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;
    my @wrap = @_
        if @_ > 0;

    @wrap = @{$_[0]}
        if ref $_[0] eq 'ARRAY';
    @wrap =  @{$this->{_text}}
        if @wrap < 1;

    my ($first_indent,$body_indent) = ($this->{_findent},$this->{_bindent});
    my ($findent,$bindent) = ($first_indent,$body_indent);

    $findent =~ s/\t/' ' x $this->{_tabs}/eg;
    $bindent =~ s/\t/' ' x $this->{_tabs}/eg;

    ($first_indent,$body_indent) = ($findent,$bindent)
        if $this->{_align} && ! $this->{_fill}
            || $this->{_exp} || $this->{_hindent};

    my @words = split /\s+/,join ' ',@wrap;
    shift @words
        if $words[0] eq '';

    @wrap = ();
    my ($line,$width,$abbrev);
    $abbrev = 0;
    $width = $this->{_cols} - length($findent)
        - $this->{_lmargin} - $this->{_rmargin};
    $line = shift @words;
    local (*is_abbrev) = $is_abbrev;
    local (*make_line) = $make_line;
    $abbrev = $this->is_abbrev($line)
        if defined $line;
    while ($_ = shift @words) {
        if(length($_) + length($line) < $width - 1
                || ($line !~ /[.?!]['"]?$/ || $abbrev)
                && length($_) + length($line) < $width) {
            $line .= ' '
                if $line =~ /[.?!]['"]?$/ && ! $abbrev;
            $line .= ' ' . $_;
        }
        else { last; }
        $abbrev = is_abbrev($this,$_);
    }
    push @wrap,$this->make_line($line,$first_indent,$width)
        if defined $line;
    $line = $_;
    $width = $this->{_cols} - length($bindent)
        - $this->{_lmargin} - $this->{_rmargin};
    $abbrev = 0;
    $abbrev = is_abbrev($this,$line)
        if defined $line;
    for (@words) {
        if(length($_) + length($line) < $width - 1
                || ($line !~ /[.?!]['"]?$/ || $abbrev)
                && length($_) + length($line) < $width) {
            $line .= ' '
                if $line =~ /[.?!]['"]?$/ && ! $abbrev;
            $line .= ' ' . $_;
        }
        else {
            push @wrap,$this->make_line($line,$body_indent,$width);
            $line = $_;
        }
        $abbrev = is_abbrev($this,$_);
    }
    push @wrap,$this->make_line($line,$body_indent,$width)
        if defined $line;

    if($this->{_hindent} && @wrap > 0) {
        $this->{_hindcurr} = $this->{_hindtext}->[0]
            if length($this->{_hindcurr}) < 1;
        my ($fchar) = $wrap[0] =~ /(\S)/;
        my $white = index $wrap[0],$fchar;
        if($white  - $this->{_lmargin} - 1 > length($this->{_hindcurr})) {
            $white = length($this->{_hindcurr}) + $this->{_lmargin};
            $wrap[0] =~
                s/^ {$white}/' ' x $this->{_lmargin} . $this->{_hindcurr}/e;
        }
        else {
            unshift @wrap,' ' x $this->{_lmargin} . $this->{_hindcurr} . "\n";
        }
    }

    wantarray ? @wrap : join '', @wrap;
}

# format lines in text into paragraphs with each element of @wrap a paragraph
# uses Text::Format->wrap for the formatting
sub fill(\$@)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;
    my @wrap = @_
        if @_ > 0;

    @wrap = @{$_[0]}
        if ref $_[0] eq 'ARRAY';
    @wrap =  @{$this->{_text}}
        if @wrap < 1;

    my (@ret,$end,$cnt,$line);

    # if indents are same, use newline between paragraphs
    if($this->{_findent} eq $this->{_bindent} ||
            $this->{_hindent}) { $end = "\n"; }
    else { $end = ''; }

    for (@wrap) {
        $this->{_hindcurr} = $this->{_hindtext}->[$cnt]
            if $this->{_hindent};
        $line = $this->wrap($_);
        push @ret,$line . $end
            if defined $line && length $line > 0;
        ++$cnt;
    }
    chop $ret[$#ret]
        if $ret[$#ret] =~ /\n\n$/;

    wantarray ? @ret : join '',@ret;
}

# center text using spaces on left side to pad it out
# empty lines are preserved
sub center(\$@)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;
    my @center = @_
        if @_ > 0;
    @center =  @{$this->{_text}}
        if @center < 1;
    my $tabs;
    my $width = $this->{_cols} - $this->{_lmargin} - $this->{_rmargin};

    for (@center) {
        s/(?:^\s+|\s+$)|\n//g;
        s/\t/' ' x $this->{_tabs}/eg
            if $this->{_exp};
        $tabs = tr/\t//; # count tabs
        substr($_,0,0) =
                ' ' x int(($width - length($_)
                - $tabs * $this->{_tabs} + $tabs) / 2)
            if length > 0;
        substr($_,0,0) = ' ' x $this->{_lmargin}
            if length > 0;
        substr($_,length) = "\n";
    }

    wantarray ? @center : join '',@center;
}

# expand tabs to spaces
# should be similar to Text::Tabs::expand
sub expand(\$@)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;
    my @lines = @_
        if @_ > 0;
    @lines =  @{$this->{_text}}
        if @lines < 1;

    for (@lines) {
        s/\t/' ' x $this->{_tabs}/eg;
    }

    wantarray ? @lines : $lines[0];
}

# turn tabstop number of spaces into tabs
# should be similar to Text::Tabs::unexpand
sub unexpand(\$@)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;
    my @lines = $this->expand(@_);

    for (@lines) {
        s/ {$this->{_tabs}}/\t/g;
    }

    wantarray ? @lines : $lines[0];
}

# return a reference to the object, call as $text = Text::Format->new()
# can be used to clone the current reference $ntext = $text->new()
sub new()
{
    my $this = shift;
    my $ref = shift;
    my ($conf,%clone);
    %clone = %{$this}
        if ref $this;

    $conf = {
            _cols     => 72,
            _tabs     => 8,
            _findent  => "\t",
            _bindent  => '',
            _fill     => 0,
            _align    => 0,
            _lmargin  => 0,
            _rmargin  => 0,
            _exp      => 0,
            _space    => 0,
            _abbrs    => {},
            _text     => [],
            _hindent  => 0,
            _hindtext => [],
            _hindcurr => '',
    };

    if(ref $ref eq 'HASH') {
        $conf->{_cols} = $ref->{columns}
            if defined $ref->{columns};
        $conf->{_tabs} = $ref->{tabstop}
            if defined $ref->{tabstop};
        $conf->{_findent} = $ref->{firstIndent}
            if defined $ref->{firstIndent};
        $conf->{_bindent} = $ref->{bodyIndent}
            if defined $ref->{bodyIndent};
        $conf->{_fill} = $ref->{rightFill}
            if defined $ref->{rightFill};
        $conf->{_align} = $ref->{rightAlign}
            if defined $ref->{rightAlign};
        $conf->{_lmargin} = $ref->{leftMargin}
            if defined $ref->{leftMargin};
        $conf->{_rmargin} = $ref->{rightMargin}
            if defined $ref->{rightMargin};
        $conf->{_exp} = $ref->{expandTabs}
            if defined $ref->{expandTabs};
        $conf->{_space} = $ref->{extraSpace}
            if defined $ref->{extraSpace};
        $conf->{_abbrs} = $ref->{abbrevs}
            if defined $ref->{abbrevs};
        $conf->{_text} = $ref->{text}
            if defined $ref->{text};
        $conf->{_hindent} = $ref->{hangingIndent}
            if defined $ref->{hangingIndent};
        $conf->{_hindtext} = $ref->{hangingText}
            if defined $ref->{hangingText};
    }

    ref $this ? bless \%clone, ref $this : bless $conf, $this;
}

# configure all the attributes of the object
# returns the old object prior to configuration
sub config
{
    my $this = shift;
    croak "Bad method call" unless ref $this;
    my $conf = shift;
    croak "Not a reference to a hash" unless ref $conf eq 'HASH';
    my %clone = %{$this};

    $this->{_cols} = $conf->{columns}
        if defined $conf->{columns};
    $this->{_tabs} = $conf->{tabstop}
        if defined $conf->{tabstop};
    $this->{_findent} = $conf->{firstIndent}
        if defined $conf->{firstIndent};
    $this->{_bindent} = $conf->{bodyIndent}
        if defined $conf->{bodyIndent};
    $this->{_fill} = $conf->{rightFill}
        if defined $conf->{rightFill};
    $this->{_align} = $conf->{rightAlign}
        if defined $conf->{rightAlign};
    $this->{_lmargin} = $conf->{leftMargin}
        if defined $conf->{leftMargin};
    $this->{_rmargin} = $conf->{rightMargin}
        if defined $conf->{rightMargin};
    $this->{_exp} = $conf->{expandTabs}
        if defined $conf->{expandTabs};
    $this->{_space} = $conf->{extraSpace}
        if defined $conf->{extraSpace};
    $this->{_abbrs} = $conf->{abbrevs}
        if defined $conf->{abbrevs};
    $this->{_text} = $conf->{text}
        if defined $conf->{text};
    $this->{_hindent} = $conf->{hangingIndent}
        if defined $conf->{hangingIndent};
    $this->{_hindtext} = $conf->{hangingText}
        if defined $conf->{hangingText};

    bless \%clone, ref $this;
}

# set or get the column size, width of text
sub columns(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    @_ ? $this->{_cols} = abs int shift : $this->{_cols};
}

# set or get the tabstop size
sub tabstop(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    @_ ? $this->{_tabs} = abs int shift : $this->{_tabs};
}

sub firstIndent(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    @_ ? $this->{_findent} = shift : $this->{_findent};
}

sub bodyIndent(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    @_ ? $this->{_bindent} = shift : $this->{_bindent};
}

sub rightFill(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    @_ ? $this->{_fill} = abs int shift : $this->{_fill};
}

sub rightAlign(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    @_ ? $this->{_align} = abs int shift : $this->{_align};
}

sub leftMargin(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    @_ ? $this->{_lmargin} = abs int shift : $this->{_lmargin};
}

sub rightMargin(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    @_ ? $this->{_rmargin} = abs int shift : $this->{_rmargin};
}

# it's used by wrap, fill and center
sub expandTabs(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    @_ ? $this->{_exp} = abs int shift : $this->{_exp};
}

# set or get whether to put extra space after a sentence
sub extraSpace(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    @_ ? $this->{_space} = abs int shift : $this->{_space};
}

# takes a reference to your hash or takes a list of abbreviations,
# returns the INTERNAL abbreviations
sub abbrevs(\$@)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    if(ref $_[0] eq 'HASH') {
        $this->{_abbrs} = shift;
    }
    elsif(@_ > 0) {
        my %tmp;
        @{tmp{@_}} = @_;
        $this->{_abbrs} = \%tmp;
    }

    wantarray ? sort keys %abbrev : join ' ',sort keys %abbrev;
}

sub text(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;
    my $text = shift;

    $this->{_text} = $text
        if ref $text eq 'ARRAY';

    wantarray ? @{$this->{_text}} : join ' ', @{$this->{_text}};
}

sub hangingIndent(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;

    @_ ? $this->{_hindent} = abs int shift : $this->{_hindent};
}

sub hangingText(\$;$)
{
    my $this = shift;
    croak "Bad method call" unless ref $this;
    my $text = shift;

    $this->{_hindtext} = $text
        if ref $text eq 'ARRAY';

    wantarray ? @{$this->{_hindtext}} : join ' ', @{$this->{_hindtext}};
}

$make_line = sub
{
    my $this = shift;
    my ($line,$lead_white,$width) = @_;
    my $fill = '';
    my $lmargin = ' ' x $this->{_lmargin};

    $fill = ' ' x ($width - length($line))
        if $this->{_fill} && ! $this->{_align};
    $line = $lmargin . $lead_white . $line . $fill . "\n"
        if defined $line;
    substr($line,0,0) = ' ' x ($this->{_cols} - (length($line) - 1))
        if $this->{_align} && ! $this->{_fill} && defined $line;

    $line;
};

$is_abbrev = sub
{
    my $this = shift;
    my $word = shift;

    $word =~ s/\.$//; # remove period if there is one
    # if we have an abbreviation OR no space is wanted after sentence
    # endings
    return 1
        if ! $this->{_space} ||
            exists($abbrev{$word}) || exists(${$this->{_abbrs}}{$word});

    0;
};

1;
