//! The built-in syntax parsers.

use prelude::*;
use prelude::parse::*;
use expr::Global;

use std::mem;

syntaxty!(visibility_type of size 1: "Styx.Visibility");

syntax! {
    in visibility (visibility_type()):

    #[doc = "Parses a [`Visibility`]."]
    fn parse(parser: &mut Parser, diagnostics: Diags) -> Result<Visibility, Failure> {
        match parser.current_or_report(diagnostics)? {
            &Token::Ident(span, ref ident) => match ident.as_str() {
                "public"   => Ok(Visibility::Public),
                "private"  => Ok(Visibility::Private),
                "internal" => Ok(Visibility::Internal),
                _ => Err(Failure::Match(span))
            },
            tok => Err(Failure::Match(tok.span()))
        }
    }
}

syntaxty!(path_type of size (mem::size_of::<Sym>() as _): "Styx.Path");

syntax! {
    in path (path_type()):

    #[doc = "Parses a path (represented as a [`Sym`])."]
    fn parse(parser: &mut Parser, diagnostics: Diags) -> Result<Spanned<Sym>, Failure> {{
        // Read first part, or indicate that no match was made
        let (span, mut path) = match parser.current_or_report(diagnostics)? {
            &Token::Ident(span, ref ident) => (span, ident.clone()),
            tok => return Err(Failure::Match(tok.span()))
        };

        let endspan = loop {
            parser.advance();

            match parser.current_or_report(diagnostics)? {
                &Token::Ident(_, ref dot) if dot == "." => (),
                token => break token.span()
            }

            parser.advance();

            match parser.current_or_report(diagnostics)? {
                &Token::Ident(_, ref ident) => path.push_str(ident),
                token => report!(diagnostics, Diagnostic::expected_token(token.span(), "identifier"))
            }
        };

        Ok(Spanned::new(Sym::from(path), span!(span.start() => endspan.end())))
    }}
}

syntaxty!(global_type of size 0: "Styx.Intrinsics.Global");

syntax! {
    in global (global_type()):

    #[doc = "Parses a reference to a global structure."]
    fn parse(parser: &mut Parser, diagnostics: Diags) -> ParseResult {{
        let (span, result) = match parser.current_or_report(diagnostics)? {
            &Token::Ident(span, ref ident) => unsafe {
                match ident.as_str() {
                    "styxcontext" => (span, Global::context(mem::transmute(parser.context()))),
                    "styxbinder"  => (span, Global::binder(mem::transmute(parser.binder()))),
                    "styxvm"      => (span, Global::vm(mem::transmute(parser.vm()))),
                    _ => return Err(Failure::Match(span))
                }
            },

            tok => return Err(Failure::Match(tok.span()))
        };

        parser.advance();

        Ok(Expr::extension(result, span, global_type()))
    }}
}

