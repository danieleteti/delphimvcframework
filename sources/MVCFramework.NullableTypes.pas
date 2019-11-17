// ***************************************************************************
//
// TNullabletypes requires spring4d framework
//
// https://bitbucket.org/sglienke/spring4d.git
//
// Contributor: 2019 - João Antônio Duarte (joao.antonioduarte@hotmail.com)
//
// ***************************************************************************

unit MVCFramework.NullableTypes;

interface

uses
  Spring;

type
  /// <summary>Alias for nullables types of Spring</summary>
  TNullableInteger = Spring.TNullableInteger;
  TNullableInt64 = Spring.TNullableInt64;
  TNullableCurrency = Spring.TNullableCurrency;
  TNullableString = Spring.TNullableString;
  TNullableDateTime = Spring.TNullableDateTime;
  TNullableDate = Spring.Nullable<TDate>;
  TNullableTime = Spring.Nullable<TTime>;
  TNullableGuid = Spring.TNullableGuid;

implementation

end.
