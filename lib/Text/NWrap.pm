package Text::NWrap;

=head1 NAME

B<Text::NWrap> - a simple text wrapping module

=head1 SYNOPSIS

    use Text::NWrap;

    $NWrap::columns = 132;
    NWrap::wrap('    ','',@text);

=head1 DESCRIPTION

B<Text::NWrap> is a simple interface to B<Text::Format>.  If you want
more functionality just use B<Text::Format>.
B<Text::NWrap> is meant to replace B<Text::Wrap>.

=over 4

=item B<wrap> $firstIndent, $bodyIndent, @text

Wrap @text using $firstIndent for the first line's indent and $bodyIndent
for the indentation of the body of the paragraph.

=item B<$columns>

Lets you set the width of the paragraph.  Default is 72 characters wide.

=back

=head1 EXAMPLE

    use Text::NWrap;

    print NWrap::wrap('','',"hello world this is some silly example",
            " and some more text");

    use Text::NWrap qw(wrap $columns);

    $columns = 20;
    wrap("\t",'',"some text that should be wrapped");

=head1 AUTHOR

Gabor Egressy B<gabor@vmunix.com>

Copyright (c) 1998 Gabor Egressy.  All rights reserved.  All wrongs
reversed.  This program is free software; you can redistribute and/or
modify it under the same terms as Perl itself.

=cut

use Text::Format;
use strict;

use vars qw(@EXPORT_OK @ISA $VERSION $columns);
@ISA = qw(Exporter);
@EXPORT_OK = qw(wrap $columns);

BEGIN {
    $columns = 72;
}
$VERSION = '0.10';

sub wrap($$@)
{
    my ($findent,$bindent,@text) = @_;

    return ''
        if @_ < 1;

    my $text = Text::Format->new({columns => $columns,
            firstIndent => length $text->expand($findent),
            bodyIndent  => length $text->expand($bindent)}
    );

    my @ret = $text->format(@text);
    $ret[0] =~ s/^ +/$findent/;
    my $i;
    for ($i = 1;$i < @ret;++$i) {
        $ret[$i] =~ s/^ +/$bindent/;
    }

    join '',@ret;
}

1;
