{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Init
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Initialization and shutdown for LDAP programs

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Init(ldapOpen,
                 ldapInit,
                 ldapInitialize,
                 ldapSimpleBind)
where

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import LDAP.Types
import Foreign.C.Types
import LDAP.Utils
import Foreign.Marshal.Utils

#include <ldap.h>


ldapSetVersion3 :: LDAPPtr -> IO LDAPInt
ldapSetVersion3 cld =
    with ((#{const LDAP_VERSION3})::LDAPInt) $ \copt ->
    ldap_set_option cld #{const LDAP_OPT_PROTOCOL_VERSION} (castPtr copt)

{- | Preferred way to initialize a LDAP connection. 
The default port is given in 'LDAP.Constants.ldapPort'.

Could throw IOError on failure. -}
ldapInit :: String              -- ^ Host
         -> LDAPInt             -- ^ Port
         -> IO LDAP             -- ^ New LDAP Obj
ldapInit host port =
    withCString host $ \cs ->
       do cld <- cldap_init cs port
          rv <- fromLDAPPtr "ldapInit" (return cld)
          ldapSetVersion3 cld
          return rv

{- | Like 'ldapInit', but establish network connection immediately. -}
ldapOpen :: String              -- ^ Host
            -> LDAPInt          -- ^ Port
            -> IO LDAP          -- ^ New LDAP Obj
ldapOpen host port =
    withCString host (\cs ->
                      fromLDAPPtr "ldapOpen" $ cldap_open cs port)

{- | Like 'ldapInit', but accepts a URI (or whitespace/comma separated
list of URIs) which can contain a schema, a host and a port.  Besides
ldap, valid schemas are ldaps (LDAP over TLS), ldapi (LDAP over IPC),
and cldap (connectionless LDAP). -}
ldapInitialize :: String        -- ^ URI
                  -> IO LDAP    -- ^ New LDAP Obj
ldapInitialize uri =
    withCString uri $ \cs ->
    alloca $ \pp -> do
    r <- ldap_initialize pp cs
    p <- peek pp
    ldap <- fromLDAPPtr "ldapInitialize" (return p)
    _ <- checkLE "ldapInitialize" ldap (return r)
    ldapSetVersion3 p
    return ldap


{- | Bind to the remote server. -}
ldapSimpleBind :: LDAP          -- ^ LDAP Object
               -> String        -- ^ DN (Distinguished Name)
               -> String        -- ^ Password
               -> IO ()
ldapSimpleBind ld dn passwd =
    withLDAPPtr ld (\ptr ->
     withCString dn (\cdn ->
      withCString passwd (\cpasswd -> 
        do checkLE "ldapSimpleBind" ld
                            (ldap_simple_bind_s ptr cdn cpasswd)
           return ()
                         )))

foreign import ccall unsafe "ldap.h ldap_init"
  cldap_init :: CString -> CInt -> IO LDAPPtr


foreign import ccall unsafe "ldap.h ldap_open"
  cldap_open :: CString -> CInt -> IO LDAPPtr

foreign import ccall unsafe "ldap.h ldap_initialize"
  ldap_initialize :: Ptr LDAPPtr -> CString -> IO LDAPInt

foreign import ccall unsafe "ldap.h ldap_simple_bind_s"
  ldap_simple_bind_s :: LDAPPtr -> CString -> CString -> IO LDAPInt

foreign import ccall unsafe "ldap.h ldap_set_option"
  ldap_set_option :: LDAPPtr -> LDAPInt -> Ptr () -> IO LDAPInt
