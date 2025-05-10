unit libyuv;

interface

const
{$IFDEF MSWINDOWS}
  YUV_LIB = 'libyuv.dll';
{$ELSEIF DEFINED(LINUX)}
  YUV_LIB = 'libyuv.so.0';
{$ELSEIF DEFINED(MACOS64)}
  YUV_LIB = 'libyuv.pa';
  PREFIX = '';
{$ENDIF}




function ARGBToI420(const src_argb: PByte; // Source ARGB data
  src_stride_argb: Integer; // Source stride (usually width * 4 for ARGB)
  dst_y: PByte; // Destination Y plane
  dst_stride_y: Integer; // Y plane stride (usually width)
  dst_u: PByte; // Destination U plane
  dst_stride_u: Integer; // U plane stride (usually width/2)
  dst_v: PByte; // Destination V plane
  dst_stride_v: Integer; // V plane stride (usually width/2)
  width: Integer; // Image width
  height: Integer // Image height
  ): Integer; cdecl; external YUV_LIB;



function I420ToARGB(const src_y: PByte; // Source Y plane
  src_stride_y: Integer; // Y plane stride
  const src_u: PByte; // Source U plane
  src_stride_u: Integer; // U plane stride
  const src_v: PByte; // Source V plane
  src_stride_v: Integer; // V plane stride
  dst_argb: PByte; // Destination ARGB buffer
  dst_stride_argb: Integer; // Destination stride (width * 4)
  width: Integer; // Image width
  height: Integer // Image height
  ): Integer; cdecl; external YUV_LIB;



function BGRAToI420(
  const src_bgra: PByte;        // Pointer to source BGRA data
  src_stride_bgra: Integer;     // Source stride
  dst_y: PByte;                 // Pointer to destination Y plane
  dst_stride_y: Integer;        // Y plane stride
  dst_u: PByte;                 // Pointer to destination U plane
  dst_stride_u: Integer;        // U plane stride
  dst_v: PByte;                 // Pointer to destination V plane
  dst_stride_v: Integer;        // V plane stride
  width: Integer;               // Image width
  height: Integer               // Image height
): Integer; cdecl; external YUV_LIB;  // cdecl calling convention for C++ compatibility



implementation

end.
