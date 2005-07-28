module LDAP.Data (module LDAP.Data) where

#include "ldap.h"


data LDAPErrorCode = 
 LdapSuccess
 |LdapOperationsError
 |LdapProtocolError
 |LdapTimelimitExceeded
 |LdapSizelimitExceeded
 |LdapCompareFalse
 |LdapCompareTrue
 |LdapAuthMethodNotSupported
 |LdapStrongAuthNotSupported
 |LdapStrongAuthRequired
 |LdapPartialResults
 |LdapReferral
 |LdapAdminlimitExceeded
 |LdapUnavailableCriticalExtension
 |LdapConfidentialityRequired
 |LdapSaslBindInProgress
 |LdapNoSuchAttribute
 |LdapUndefinedType
 |LdapInappropriateMatching
 |LdapConstraintViolation
 |LdapTypeOrValueExists
 |LdapInvalidSyntax
 |LdapNoSuchObject
 |LdapAliasProblem
 |LdapInvalidDnSyntax
 |LdapIsLeaf
 |LdapAliasDerefProblem
 |LdapProxyAuthzFailure
 |LdapInappropriateAuth
 |LdapInvalidCredentials
 |LdapInsufficientAccess
 |LdapBusy
 |LdapUnavailable
 |LdapUnwillingToPerform
 |LdapLoopDetect
 |LdapNamingViolation
 |LdapObjectClassViolation
 |LdapNotAllowedOnNonleaf
 |LdapNotAllowedOnRdn
 |LdapAlreadyExists
 |LdapNoObjectClassMods
 |LdapResultsTooLarge
 |LdapAffectsMultipleDsas
 |LdapOther
 |LdapServerDown
 |LdapLocalError
 |LdapEncodingError
 |LdapDecodingError
 |LdapTimeout
 |LdapAuthUnknown
 |LdapFilterError
 |LdapUserCancelled
 |LdapParamError
 |LdapNoMemory
 |LdapConnectError
 |LdapNotSupported
 |LdapControlNotFound
 |LdapNoResultsReturned
 |LdapMoreResultsToReturn
 |LdapClientLoop
 |LdapReferralLimitExceeded
 deriving (Eq, Bounded, Show)

instance Enum LDAPErrorCode where
 toEnum #{const LDAP_SUCCESS} = LdapSuccess
 toEnum #{const LDAP_OPERATIONS_ERROR} = LdapOperationsError
 toEnum #{const LDAP_PROTOCOL_ERROR} = LdapProtocolError
 toEnum #{const LDAP_TIMELIMIT_EXCEEDED} = LdapTimelimitExceeded
 toEnum #{const LDAP_SIZELIMIT_EXCEEDED} = LdapSizelimitExceeded
 toEnum #{const LDAP_COMPARE_FALSE} = LdapCompareFalse
 toEnum #{const LDAP_COMPARE_TRUE} = LdapCompareTrue
 toEnum #{const LDAP_AUTH_METHOD_NOT_SUPPORTED} = LdapAuthMethodNotSupported
 toEnum #{const LDAP_STRONG_AUTH_NOT_SUPPORTED} = LdapStrongAuthNotSupported
 toEnum #{const LDAP_STRONG_AUTH_REQUIRED} = LdapStrongAuthRequired
 toEnum #{const LDAP_PARTIAL_RESULTS} = LdapPartialResults
 toEnum #{const LDAP_REFERRAL} = LdapReferral
 toEnum #{const LDAP_ADMINLIMIT_EXCEEDED} = LdapAdminlimitExceeded
 toEnum #{const LDAP_UNAVAILABLE_CRITICAL_EXTENSION} = LdapUnavailableCriticalExtension
 toEnum #{const LDAP_CONFIDENTIALITY_REQUIRED} = LdapConfidentialityRequired
 toEnum #{const LDAP_SASL_BIND_IN_PROGRESS} = LdapSaslBindInProgress
 toEnum #{const LDAP_NO_SUCH_ATTRIBUTE} = LdapNoSuchAttribute
 toEnum #{const LDAP_UNDEFINED_TYPE} = LdapUndefinedType
 toEnum #{const LDAP_INAPPROPRIATE_MATCHING} = LdapInappropriateMatching
 toEnum #{const LDAP_CONSTRAINT_VIOLATION} = LdapConstraintViolation
 toEnum #{const LDAP_TYPE_OR_VALUE_EXISTS} = LdapTypeOrValueExists
 toEnum #{const LDAP_INVALID_SYNTAX} = LdapInvalidSyntax
 toEnum #{const LDAP_NO_SUCH_OBJECT} = LdapNoSuchObject
 toEnum #{const LDAP_ALIAS_PROBLEM} = LdapAliasProblem
 toEnum #{const LDAP_INVALID_DN_SYNTAX} = LdapInvalidDnSyntax
 toEnum #{const LDAP_IS_LEAF} = LdapIsLeaf
 toEnum #{const LDAP_ALIAS_DEREF_PROBLEM} = LdapAliasDerefProblem
 toEnum #{const LDAP_PROXY_AUTHZ_FAILURE} = LdapProxyAuthzFailure
 toEnum #{const LDAP_INAPPROPRIATE_AUTH} = LdapInappropriateAuth
 toEnum #{const LDAP_INVALID_CREDENTIALS} = LdapInvalidCredentials
 toEnum #{const LDAP_INSUFFICIENT_ACCESS} = LdapInsufficientAccess
 toEnum #{const LDAP_BUSY} = LdapBusy
 toEnum #{const LDAP_UNAVAILABLE} = LdapUnavailable
 toEnum #{const LDAP_UNWILLING_TO_PERFORM} = LdapUnwillingToPerform
 toEnum #{const LDAP_LOOP_DETECT} = LdapLoopDetect
 toEnum #{const LDAP_NAMING_VIOLATION} = LdapNamingViolation
 toEnum #{const LDAP_OBJECT_CLASS_VIOLATION} = LdapObjectClassViolation
 toEnum #{const LDAP_NOT_ALLOWED_ON_NONLEAF} = LdapNotAllowedOnNonleaf
 toEnum #{const LDAP_NOT_ALLOWED_ON_RDN} = LdapNotAllowedOnRdn
 toEnum #{const LDAP_ALREADY_EXISTS} = LdapAlreadyExists
 toEnum #{const LDAP_NO_OBJECT_CLASS_MODS} = LdapNoObjectClassMods
 toEnum #{const LDAP_RESULTS_TOO_LARGE} = LdapResultsTooLarge
 toEnum #{const LDAP_AFFECTS_MULTIPLE_DSAS} = LdapAffectsMultipleDsas
 toEnum #{const LDAP_OTHER} = LdapOther
 toEnum #{const LDAP_SERVER_DOWN} = LdapServerDown
 toEnum #{const LDAP_LOCAL_ERROR} = LdapLocalError
 toEnum #{const LDAP_ENCODING_ERROR} = LdapEncodingError
 toEnum #{const LDAP_DECODING_ERROR} = LdapDecodingError
 toEnum #{const LDAP_TIMEOUT} = LdapTimeout
 toEnum #{const LDAP_AUTH_UNKNOWN} = LdapAuthUnknown
 toEnum #{const LDAP_FILTER_ERROR} = LdapFilterError
 toEnum #{const LDAP_USER_CANCELLED} = LdapUserCancelled
 toEnum #{const LDAP_PARAM_ERROR} = LdapParamError
 toEnum #{const LDAP_NO_MEMORY} = LdapNoMemory
 toEnum #{const LDAP_CONNECT_ERROR} = LdapConnectError
 toEnum #{const LDAP_NOT_SUPPORTED} = LdapNotSupported
 toEnum #{const LDAP_CONTROL_NOT_FOUND} = LdapControlNotFound
 toEnum #{const LDAP_NO_RESULTS_RETURNED} = LdapNoResultsReturned
 toEnum #{const LDAP_MORE_RESULTS_TO_RETURN} = LdapMoreResultsToReturn
 toEnum #{const LDAP_CLIENT_LOOP} = LdapClientLoop
 toEnum #{const LDAP_REFERRAL_LIMIT_EXCEEDED} = LdapReferralLimitExceeded
 toEnum x = error $ "Code " ++ show x ++ " is not a valid LDAPErrorCode"

 fromEnum LdapSuccess = #{const LDAP_SUCCESS}
 fromEnum LdapOperationsError = #{const LDAP_OPERATIONS_ERROR}
 fromEnum LdapProtocolError = #{const LDAP_PROTOCOL_ERROR}
 fromEnum LdapTimelimitExceeded = #{const LDAP_TIMELIMIT_EXCEEDED}
 fromEnum LdapSizelimitExceeded = #{const LDAP_SIZELIMIT_EXCEEDED}
 fromEnum LdapCompareFalse = #{const LDAP_COMPARE_FALSE}
 fromEnum LdapCompareTrue = #{const LDAP_COMPARE_TRUE}
 fromEnum LdapAuthMethodNotSupported = #{const LDAP_AUTH_METHOD_NOT_SUPPORTED}
 fromEnum LdapStrongAuthNotSupported = #{const LDAP_STRONG_AUTH_NOT_SUPPORTED}
 fromEnum LdapStrongAuthRequired = #{const LDAP_STRONG_AUTH_REQUIRED}
 fromEnum LdapPartialResults = #{const LDAP_PARTIAL_RESULTS}
 fromEnum LdapReferral = #{const LDAP_REFERRAL}
 fromEnum LdapAdminlimitExceeded = #{const LDAP_ADMINLIMIT_EXCEEDED}
 fromEnum LdapUnavailableCriticalExtension = #{const LDAP_UNAVAILABLE_CRITICAL_EXTENSION}
 fromEnum LdapConfidentialityRequired = #{const LDAP_CONFIDENTIALITY_REQUIRED}
 fromEnum LdapSaslBindInProgress = #{const LDAP_SASL_BIND_IN_PROGRESS}
 fromEnum LdapNoSuchAttribute = #{const LDAP_NO_SUCH_ATTRIBUTE}
 fromEnum LdapUndefinedType = #{const LDAP_UNDEFINED_TYPE}
 fromEnum LdapInappropriateMatching = #{const LDAP_INAPPROPRIATE_MATCHING}
 fromEnum LdapConstraintViolation = #{const LDAP_CONSTRAINT_VIOLATION}
 fromEnum LdapTypeOrValueExists = #{const LDAP_TYPE_OR_VALUE_EXISTS}
 fromEnum LdapInvalidSyntax = #{const LDAP_INVALID_SYNTAX}
 fromEnum LdapNoSuchObject = #{const LDAP_NO_SUCH_OBJECT}
 fromEnum LdapAliasProblem = #{const LDAP_ALIAS_PROBLEM}
 fromEnum LdapInvalidDnSyntax = #{const LDAP_INVALID_DN_SYNTAX}
 fromEnum LdapIsLeaf = #{const LDAP_IS_LEAF}
 fromEnum LdapAliasDerefProblem = #{const LDAP_ALIAS_DEREF_PROBLEM}
 fromEnum LdapProxyAuthzFailure = #{const LDAP_PROXY_AUTHZ_FAILURE}
 fromEnum LdapInappropriateAuth = #{const LDAP_INAPPROPRIATE_AUTH}
 fromEnum LdapInvalidCredentials = #{const LDAP_INVALID_CREDENTIALS}
 fromEnum LdapInsufficientAccess = #{const LDAP_INSUFFICIENT_ACCESS}
 fromEnum LdapBusy = #{const LDAP_BUSY}
 fromEnum LdapUnavailable = #{const LDAP_UNAVAILABLE}
 fromEnum LdapUnwillingToPerform = #{const LDAP_UNWILLING_TO_PERFORM}
 fromEnum LdapLoopDetect = #{const LDAP_LOOP_DETECT}
 fromEnum LdapNamingViolation = #{const LDAP_NAMING_VIOLATION}
 fromEnum LdapObjectClassViolation = #{const LDAP_OBJECT_CLASS_VIOLATION}
 fromEnum LdapNotAllowedOnNonleaf = #{const LDAP_NOT_ALLOWED_ON_NONLEAF}
 fromEnum LdapNotAllowedOnRdn = #{const LDAP_NOT_ALLOWED_ON_RDN}
 fromEnum LdapAlreadyExists = #{const LDAP_ALREADY_EXISTS}
 fromEnum LdapNoObjectClassMods = #{const LDAP_NO_OBJECT_CLASS_MODS}
 fromEnum LdapResultsTooLarge = #{const LDAP_RESULTS_TOO_LARGE}
 fromEnum LdapAffectsMultipleDsas = #{const LDAP_AFFECTS_MULTIPLE_DSAS}
 fromEnum LdapOther = #{const LDAP_OTHER}
 fromEnum LdapServerDown = #{const LDAP_SERVER_DOWN}
 fromEnum LdapLocalError = #{const LDAP_LOCAL_ERROR}
 fromEnum LdapEncodingError = #{const LDAP_ENCODING_ERROR}
 fromEnum LdapDecodingError = #{const LDAP_DECODING_ERROR}
 fromEnum LdapTimeout = #{const LDAP_TIMEOUT}
 fromEnum LdapAuthUnknown = #{const LDAP_AUTH_UNKNOWN}
 fromEnum LdapFilterError = #{const LDAP_FILTER_ERROR}
 fromEnum LdapUserCancelled = #{const LDAP_USER_CANCELLED}
 fromEnum LdapParamError = #{const LDAP_PARAM_ERROR}
 fromEnum LdapNoMemory = #{const LDAP_NO_MEMORY}
 fromEnum LdapConnectError = #{const LDAP_CONNECT_ERROR}
 fromEnum LdapNotSupported = #{const LDAP_NOT_SUPPORTED}
 fromEnum LdapControlNotFound = #{const LDAP_CONTROL_NOT_FOUND}
 fromEnum LdapNoResultsReturned = #{const LDAP_NO_RESULTS_RETURNED}
 fromEnum LdapMoreResultsToReturn = #{const LDAP_MORE_RESULTS_TO_RETURN}
 fromEnum LdapClientLoop = #{const LDAP_CLIENT_LOOP}
 fromEnum LdapReferralLimitExceeded = #{const LDAP_REFERRAL_LIMIT_EXCEEDED}
instance Ord LDAPErrorCode where
 compare x y = compare (fromEnum x) (fromEnum y)


