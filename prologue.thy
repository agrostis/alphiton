\comment -*-tex-*-

\comment -- Unprintable character aliases

\alias\carriagereturn{}
\alias\linefeed{
}
\alias\blank{ }
\alias\htab{	}
\alias\nbsp{ }
{\setcat\lbrace[\setcat\rbrace]\setcat\escape//
 /setcat/other{/setcat/other}/setcat/other\/
 /global/alias/backslash\/
 /global/alias/leftbrace{/
 /global/alias/rightbrace}/
 /setcat/rbrace}/
}

\comment -- English error messages

\newlang{en_US}
\lcdef\errmsg\emptyCondition{Empty condition}
\lcdef\errmsg\eotInCommandToken{EOT occurred while parsing command token}
\lcdef\errmsg\eotInGroup{EOT occurred inside a group}
\lcdef\errmsg\eotInParamToken{EOT occurred while parsing parameter token}
\lcdef\errmsg\errorsInAliasOrEdef{Errors in \backslash alias or\
               \backslash edef expansion}
\lcdef\errmsg\errorsInError{Errors in \backslash error expansion}
\lcdef\errmsg\extraConditionalBranch{Extra branch after \backslash else\
               in conditional}
\lcdef\errmsg\hangingRbrace{Hanging right brace}
\lcdef\errmsg\internalError#M{Internal error in builtin expansion: #M}
\lcdef\errmsg\invalidLocale{Invalid locale id}
\lcdef\errmsg\invalidStep{Invalid iteration step value}
\lcdef\errmsg\missingCondition{Missing \backslash elsif... \
               or \backslash else... in conditional}
\lcdef\errmsg\missingConditionalBody{Missing \backslash then... \
               in conditional}
\lcdef\errmsg\noContextInput{Input has no associated context}
\lcdef\errmsg\noData{No data received for register initializer, condition,\
               or in a similar context}
\lcdef\errmsg\noLocale{Current locale unset}
\lcdef\errmsg\noMatchingDef{No matching definition}
\lcdef\errmsg\patternMismatch{Pattern mismatch}
\lcdef\errmsg\stackOpInvalidArgs#D{Data on stack are not valid arguments\
               for this operation: #D}
\lcdef\errmsg\stackUnderflow{Stack underflow}
\lcdef\errmsg\undefined{Undefined command}
\lcdef\errmsg\unfinishedDef{Unfinished definition}


\comment -- Group delimiters as commands

\def\bgroup{\noexpand{}
\def\egroup{\noexpand}}


\comment -- Make blanks, tabs and newlines insignificant

\def\insignificantWhitespaces{\local\setcat\invalid\blank\local\setcat\invalid\linefeed\local\setcat\invalid\carriagereturn\local\setcat\invalid\htab}
