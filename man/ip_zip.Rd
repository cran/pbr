% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ip_zip}
\alias{ip_zip}
\alias{get_ip_address}
\alias{get_zip}
\title{Functions to retrieve IP address and ZIP code from IP}
\usage{
get_ip_address()

get_zip()
}
\value{
Either the IP address that a web browser sees
(\code{get_ip_address}), or a zip code (\code{get_zip})
}
\description{
These are just helper shortcut functions. \code{get_ip_address}
retrieves a computer's IP address from \url{https://ipinfo.io/what-is-my-ip}.
This is easier than getting the IP right off local computer because of bogon
IP addresses. \code{get_zip} pulls ZIP code location from \url{https://ipapi.co/}
}
