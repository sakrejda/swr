% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statements.R
\name{SingleStatementType}
\alias{SingleStatementType}
\title{Class representing a single Stan statement, not user-facing}
\value{
class representing the statement
}
\description{
Class representing a single Stan statement, not user-facing

Class representing a single Stan statement, not user-facing
}
\section{Super class}{
\code{\link[swr:BareStatementType]{swr::BareStatementType}} -> \code{SingleStatementType}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{SingleStatementType$new()}}
\item \href{#method-textify_declarations}{\code{SingleStatementType$textify_declarations()}}
\item \href{#method-textify_statements}{\code{SingleStatementType$textify_statements()}}
\item \href{#method-statements}{\code{SingleStatementType$statements()}}
\item \href{#method-declarations}{\code{SingleStatementType$declarations()}}
\item \href{#method-clone}{\code{SingleStatementType$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SingleStatementType$new(x, declarations = rlang::env(), indent = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{expression to treat as Stan statement}

\item{\code{declarations}}{environment to use to create declarations}

\item{\code{indent}}{integer indicating indentation level in Stan code...}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-textify_declarations"></a>}}
\if{latex}{\out{\hypertarget{method-textify_declarations}{}}}
\subsection{Method \code{textify_declarations()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SingleStatementType$textify_declarations()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-textify_statements"></a>}}
\if{latex}{\out{\hypertarget{method-textify_statements}{}}}
\subsection{Method \code{textify_statements()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SingleStatementType$textify_statements()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-statements"></a>}}
\if{latex}{\out{\hypertarget{method-statements}{}}}
\subsection{Method \code{statements()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SingleStatementType$statements()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-declarations"></a>}}
\if{latex}{\out{\hypertarget{method-declarations}{}}}
\subsection{Method \code{declarations()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SingleStatementType$declarations()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SingleStatementType$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
