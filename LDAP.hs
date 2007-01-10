{- -*- Mode: haskell; -*-
Haskell LDAP Interface

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP
   Copyright  : Copyright (C) 2005-2007 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Top-level LDAP module.

Written by John Goerzen, jgoerzen\@complete.org

Welcome to the LDAP interface for Haskell.  Please see one of the sections
below for more information.

This package comes from:

<http://software.complete.org/ldap-haskell>
-}

module LDAP (-- * Basic Types
             module LDAP.Types,
             -- * Initialization
             module LDAP.Init,
             -- * Searching
             module LDAP.Search,
             -- * Adding, Deleting, and Altering
             module LDAP.Modify,
             -- * Error Handling
             module LDAP.Exceptions,
             -- * Haskell enumerated LDAP types
             module LDAP.Data,
             -- * Other LDAP constants
             module LDAP.Constants
            )
where
import LDAP.Exceptions
import LDAP.Types
import LDAP.Init
import LDAP.Data
import LDAP.Constants
import LDAP.Search hiding (LDAPScope(..))
import LDAP.Modify hiding (LDAPModOp(..))

