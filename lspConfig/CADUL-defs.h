
#define far
#define near
#define FAR
#define NEAR

#define COMPACT 1
#define size_t unsigned int
#define _T_SIZE 1
#define selector WORD
#define CADUL 1
#define _T_WCHAR
#define BOOL bool
#define TRUE true
#define FALSE false

#define GENESIS_EXPORT(a,b,c,d) do{}while(0);

#define _inline CADUL_INLINE
extern "C"
{
    void volatile CADUL_INLINE(unsigned char uc);


    void set_reg_eax(unsigned int);
    void set_reg_ebx(unsigned int);
    void set_reg_ecx(unsigned int);
    void set_reg_edx(unsigned int);
    void set_reg_edi(unsigned int);
    void set_reg_esi(unsigned int);
    unsigned int get_reg_eax();
    unsigned int get_reg_ebx();
    unsigned int get_reg_ecx();
    unsigned int get_reg_edx();
    unsigned int get_edi_edx();
    unsigned int get_esi_edx();

    typedef unsigned short Selector;
}

 /*(
 * The information  contained  herein is a trade secret of CAD-UL GmbH,
 * Ulm, Germany, and is confidential information. It is provided  under
 * a license agreement, and may be  copied or disclosed  only under the
 * terms of  that  agreement.  Any  reproduction or disclosure  of this
 * material  without  the express written  authorization of CAD-UL GmbH
 * or persuant to the license agreement is unlawful.
 *
 * Copyright (c) 1991-1998 CAD-UL GmbH
 * An unpublished work by CAD-UL GmbH, Germany
 * All rights reserved.
 *
 *
 * $Log: xstddef,v $
 * Revision 1.2  2000/02/08 06:21:07  pla
 * LIBCXX V210
 *
 * Revision 1.1  1998/05/04 14:43:07  aki
 * Initial revision
 *
 */

// xstddef standard header
#ifndef _XSTDDEF_
#define _XSTDDEF_

#pragma noarguments_in_registers(push)

#ifndef _YVALS
 #include <yvals.h>
#endif
#include <cstddef>
_STD_BEGIN
		// EXCEPTION MACROS
 #if _HAS_EXCEPTIONS
 #define _TRY_BEGIN	try {
 #define _CATCH(x)	} catch (x) {
 #define _CATCH_ALL	} catch (...) {
 #define _CATCH_END	}
 #define _RAISE(x)	throw (x)
 #define _RERAISE	throw
 #define _THROW0()	throw ()
 #define _THROW1(x)	throw (x)
 #define _THROW(x, y)	throw x(y)
 #else	/* no exceptions */
 #define _TRY_BEGIN	{{
 #define _CATCH(x)	} if (0) {
 #define _CATCH_ALL	} if (0) {
 #define _CATCH_END	}}
 #define _RAISE(x)	_Throw(x)
 #define _RERAISE
 #define _THROW0()
 #define _THROW1(x)
 #define _THROW(x, y)	x(y)._Raise()
 #endif /* _HAS_EXCEPTIONS */
		// explicit KEYWORD
		// KEYWORD
 #define _TEMPLATE
 #define typename typename
 #ifdef _TEMPLATE_STAT
  #define _TEMPLATE_MEMBER
 #else
  #define _TEMPLATE_STAT	template<>
  #define _TEMPLATE_MEMBER	template
 #endif
		// BITMASK MACROS
 #define _BITMASK(E, T)	typedef int T
 #define _BITMASK_OPS(T)
		// MISCELLANEOUS MACROS
#define _DESTRUCTOR(ty, ptr)	(ptr)->~ty()
#define _MESG(str)	str
#define _PROTECTED	protected
 #define _TDEF(x)
 #define _TDEF2(x, y)
 #define _CNTSIZ(iter)	ptrdiff_t
 #define _STCONS(ty, name, val)	static const ty name = val
 #define _CSTD	::
		// TYPE DEFINITIONS
enum _Uninitialized {_Noinit};
		// FUNCTIONS
void _Nomemory();
_STD_END

#pragma arguments_in_registers(pop)

#endif /* _XSTDDEF_ */

/*
 * Copyright (c) 1995 by P.J. Plauger.  ALL RIGHTS RESERVED.
 * Consult your license regarding permissions and restrictions.
V2.2:0414 */


 /*(
 * The information  contained  herein is a trade secret of CAD-UL GmbH,
 * Ulm, Germany, and is confidential information. It is provided  under
 * a license agreement, and may be  copied or disclosed  only under the
 * terms of  that  agreement.  Any  reproduction or disclosure  of this
 * material  without  the express written  authorization of CAD-UL GmbH
 * or persuant to the license agreement is unlawful.
 *
 * Copyright (c) 1991-1998 CAD-UL GmbH
 * An unpublished work by CAD-UL GmbH, Germany
 * All rights reserved.
 *
 *
 * $Log: xutility,v $
 * Revision 1.2  2000/02/08 06:21:08  pla
 * LIBCXX V210
 *
 * Revision 1.1  1998/05/04 14:43:07  aki
 * Initial revision
 *
 */

// xutility internal header
#ifndef _XUTILITY_
#define _XUTILITY_


#include <utility>
_STD_BEGIN
//	ITERATOR STUFF (from <iterator>)
		// ITERATOR TAGS
struct input_iterator_tag {};
struct output_iterator_tag {};
struct forward_iterator_tag
	: public input_iterator_tag {};
struct bidirectional_iterator_tag
	: public forward_iterator_tag {};
struct random_access_iterator_tag
	: public bidirectional_iterator_tag  {};
struct _Int_iterator_tag {};
		// TEMPLATE CLASS iterator
template<class __C, class _Ty, class _D = ptrdiff_t,
	class _Pt = _Ty *, class _Rt = _Ty&>
	struct iterator {
	typedef __C iterator_category;
	typedef _Ty value_type;
	typedef _D difference_type;
	typedef _Pt pointer;
	typedef _Rt reference;
	};
template<class _Ty, class _D, class _Pt, class _Rt>
	struct _Bidit : public iterator<bidirectional_iterator_tag,
		_Ty, _D, _Pt, _Rt> {};
template<class _Ty, class _D, class _Pt, class _Rt>
	struct _Ranit : public iterator<random_access_iterator_tag,
		_Ty, _D, _Pt, _Rt> {};
struct _Outit : public iterator<output_iterator_tag,
	void, void, void, void> {};
		// TEMPLATE CLASS iterator_traits
template<class _It>
	struct iterator_traits {
	typedef typename _It::iterator_category iterator_category;
	typedef typename _It::value_type value_type;
	typedef typename _It::difference_type difference_type;
	typedef typename _It::pointer pointer;
	typedef typename _It::reference reference;
	};
		// TEMPLATE FUNCTION _Iter_cat
template<class __C, class _Ty, class _D,
	class _Pt, class _Rt> inline
	__C _Iter_cat(const iterator<__C, _Ty, _D, _Pt, _Rt>&)
	{__C __X;
	return (__X); }
template<class _Ty> inline
	random_access_iterator_tag _Iter_cat(const _Ty *)
	{random_access_iterator_tag __X;
	return (__X); }
		// INTEGER FUNCTION _Iter_cat
inline _Int_iterator_tag _Iter_cat(_Bool)
	{_Int_iterator_tag __X;
	return (__X); }
inline _Int_iterator_tag _Iter_cat(char)
	{_Int_iterator_tag __X;
	return (__X); }
inline _Int_iterator_tag _Iter_cat(signed char)
	{_Int_iterator_tag __X;
	return (__X); }
inline _Int_iterator_tag _Iter_cat(unsigned char)
	{_Int_iterator_tag __X;
	return (__X); }
inline _Int_iterator_tag _Iter_cat(wchar_t)
	{_Int_iterator_tag __X;
	return (__X); }
inline _Int_iterator_tag _Iter_cat(short)
	{_Int_iterator_tag __X;
	return (__X); }
inline _Int_iterator_tag _Iter_cat(unsigned short)
	{_Int_iterator_tag __X;
	return (__X); }
inline _Int_iterator_tag _Iter_cat(int)
	{_Int_iterator_tag __X;
	return (__X); }
inline _Int_iterator_tag _Iter_cat(unsigned int)
	{_Int_iterator_tag __X;
	return (__X); }
inline _Int_iterator_tag _Iter_cat(long)
	{_Int_iterator_tag __X;
	return (__X); }
inline _Int_iterator_tag _Iter_cat(unsigned long)
	{_Int_iterator_tag __X;
	return (__X); }
		// TEMPLATE FUNCTION _Distance
template<class _II> inline
	_CNTSIZ(_II) distance(_II _F, _II __L)
	{_CNTSIZ(_II) __N = 0;
	_Distance2(_F, __L, __N, _Iter_cat(_F));
	return (__N); }
template<class _II, class _D> inline
	void _Distance(_II _F, _II __L, _D& __N)
	{_Distance2(_F, __L, __N, _Iter_cat(_F)); }
template<class _II, class _D> inline
	void _Distance2(_II _F, _II __L, _D& __N,
		input_iterator_tag)
	{for (; _F != __L; ++_F)
		++__N; }
template<class _II, class _D> inline
	void _Distance2(_II _F, _II __L, _D& __N,
		forward_iterator_tag)
	{for (; _F != __L; ++_F)
		++__N; }
template<class _II, class _D> inline
	void _Distance2(_II _F, _II __L, _D& __N,
		bidirectional_iterator_tag)
	{for (; _F != __L; ++_F)
		++__N; }
template<class _RI, class _D> inline
	void _Distance2(_RI _F, _RI __L, _D& __N,
		random_access_iterator_tag)
	{__N += __L - _F; }
		// TEMPLATE CLASS _Ptrit
template<class _Ty, class _D, class _Pt, class _Rt,
	class _Pt2, class _Rt2>
	class _Ptrit : public iterator<random_access_iterator_tag,
		_Ty, _D, _Pt, _Rt> {
public:
	typedef _Ptrit<_Ty, _D, _Pt, _Rt, _Pt2, _Rt2> _Myt;
	_Ptrit()
		{}
	explicit _Ptrit(_Pt __P)
		: current(__P) {}
	_Ptrit(const _Ptrit<_Ty, _D, _Pt2, _Rt2, _Pt2, _Rt2>& __X)
		: current(__X.base()) {}
	_Pt base() const
		{return (current); }
	_Rt operator*() const
		{return (*current); }
	_Myt& operator++()
		{++current;
		return (*this); }
	_Myt operator++(int)
		{_Myt _Tmp = *this;
		++current;
		return (_Tmp); }
	_Myt& operator--()
		{--current;
		return (*this); }
	_Myt operator--(int)
		{_Myt _Tmp = *this;
		--current;
		return (_Tmp); }
	bool operator==(int _Y) const
		{return (current == _Y); }
	bool operator==(const _Myt& _Y) const
		{return (current == _Y.current); }
	bool operator!=(const _Myt& _Y) const
		{return (!(*this == _Y)); }
	_Myt& operator+=(_D __N)
		{current += __N;
		return (*this); }
	_Myt operator+(_D __N) const
		{return (_Myt(current + __N)); }
	_Myt& operator-=(_D __N)
		{current -= __N;
		return (*this); }
	_Myt operator-(_D __N) const
		{return (_Myt(current - __N)); }
	_Rt operator[](_D __N) const
		{return (*(*this + __N)); }
	bool operator<(const _Myt& _Y) const
		{return (current < _Y.current); }
	bool operator>(const _Myt& _Y) const
		{return (_Y < *this); }
	bool operator<=(const _Myt& _Y) const
		{return (!(_Y < *this)); }
	bool operator>=(const _Myt& _Y) const
		{return (!(*this < _Y)); }
	_D operator-(const _Myt& _Y) const
		{return (current - _Y.current); }
protected:
	_Pt current;
	};
template<class _Ty, class _D, class _Pt, class _Rt,
	class _Pt2, class _Rt2> inline
	_Ptrit<_Ty, _D, _Pt, _Rt, _Pt2, _Rt2>
		operator+(_D __N,
			const _Ptrit<_Ty, _D, _Pt, _Rt, _Pt2, _Rt2>& _Y)
	{return (_Y + __N); }
		// TEMPLATE CLASS reverse_iterator
template<class _RI>
	class reverse_iterator : public iterator<
		typename iterator_traits<_RI>::iterator_category,
		typename iterator_traits<_RI>::value_type,
		typename iterator_traits<_RI>::difference_type,
		typename iterator_traits<_RI>::pointer,
		typename iterator_traits<_RI>::reference> {
public:
	typedef reverse_iterator<_RI> _Myt;
	typedef typename iterator_traits<_RI>::difference_type _D;
	typedef typename iterator_traits<_RI>::pointer _Pt;
	typedef typename iterator_traits<_RI>::reference _Rt;
	typedef _RI iterator_type;
	reverse_iterator()
		{}
	explicit reverse_iterator(_RI __X)
		: current(__X) {}
	template<class __U>
		reverse_iterator(const reverse_iterator<__U>& __X)
		: current(__X.base()) {}
	_RI base() const
		{return (current); }
	_Rt operator*() const
		{_RI _Tmp = current;
		return (*--_Tmp); }
	_Myt& operator++()
		{--current;
		return (*this); }
	_Myt operator++(int)
		{_Myt _Tmp = *this;
		--current;
		return (_Tmp); }
	_Myt& operator--()
		{++current;
		return (*this); }
	_Myt operator--(int)
		{_Myt _Tmp = *this;
		++current;
		return (_Tmp); }
	bool _Eq(const _Myt& _Y) const
		{return (current == _Y.current); }
		// random-access only beyond this point
	_Myt& operator+=(_D __N)
		{current -= __N;
		return (*this); }
	_Myt operator+(_D __N) const
		{return (_Myt(current - __N)); }
	_Myt& operator-=(_D __N)
		{current += __N;
		return (*this); }
	_Myt operator-(_D __N) const
		{return (_Myt(current + __N)); }
	_Rt operator[](_D __N) const
		{return (*(*this + __N)); }
	bool _Lt(const _Myt& _Y) const
		{return (_Y.current < current); }
	_D _Mi(const _Myt& _Y) const
		{return (_Y.current - current); }
protected:
	_RI current;
	};
template<class _RI, class _D> inline
	reverse_iterator<_RI> operator+(_D __N,
		const reverse_iterator<_RI>& _Y)
	{return (_Y + __N); }
template<class _RI> inline
	typename reverse_iterator<_RI>::_D
		operator-(const reverse_iterator<_RI>& __X,
		const reverse_iterator<_RI>& _Y)
	{return (__X._Mi(_Y)); }
template<class _RI> inline
	bool operator==(const reverse_iterator<_RI>& __X,
		const reverse_iterator<_RI>& _Y)
	{return (__X._Eq(_Y)); }
template<class _RI> inline
	bool operator!=(const reverse_iterator<_RI>& __X,
		const reverse_iterator<_RI>& _Y)
	{return (!(__X == _Y)); }
template<class _RI> inline
	bool operator<(const reverse_iterator<_RI>& __X,
		const reverse_iterator<_RI>& _Y)
	{return (__X._Lt(_Y)); }
template<class _RI> inline
	bool operator>(const reverse_iterator<_RI>& __X,
		const reverse_iterator<_RI>& _Y)
	{return (_Y < __X); }
template<class _RI> inline
	bool operator<=(const reverse_iterator<_RI>& __X,
		const reverse_iterator<_RI>& _Y)
	{return (!(_Y < __X)); }
template<class _RI> inline
	bool operator>=(const reverse_iterator<_RI>& __X,
		const reverse_iterator<_RI>& _Y)
	{return (!(__X < _Y)); }
		// TEMPLATE CLASS istreambuf_iterator
template<class _E, class _Tr = char_traits>
	class istreambuf_iterator
		: public iterator<input_iterator_tag,
			_E, typename _Tr::off_type, _E *, _E&> {
public:
	typedef istreambuf_iterator<_E, _Tr> _Myt;
	typedef char char_type;
	typedef char_traits traits_type;
	typedef streambuf streambuf_type;
	typedef istream istream_type;
	typedef typename traits_type::int_type int_type;
	istreambuf_iterator(streambuf_type *_Sb = 0) _THROW0()
		: _Sbuf(_Sb), _Got(_Sb == 0) {}
	istreambuf_iterator(istream_type& _I) _THROW0()
		: _Sbuf(_I.rdbuf()), _Got(_I.rdbuf() == 0) {}
	const _E& operator*() const
		{if (!_Got)
			((_Myt *)this)->_Peek();
		return (_Val); }
	_Myt& operator++()
		{_Inc();
		return (*this); }
	_Myt operator++(int)
		{if (!_Got)
			_Peek();
		_Myt _Tmp = *this;
		_Inc();
		return (_Tmp); }
	bool equal(const _Myt& __X) const
		{if (!_Got)
			((_Myt *)this)->_Peek();
		if (!__X._Got)
			((_Myt *)&__X)->_Peek();
		return (_Sbuf == 0 && __X._Sbuf == 0
			|| _Sbuf != 0 && __X._Sbuf != 0); }
private:
	void _Inc()
		{if (_Sbuf == 0
			|| traits_type::eq_int_type(traits_type::eof(),
				_Sbuf->sbumpc()))
			_Sbuf = 0, _Got = true;
		else
			_Got = false; }
	_E _Peek()
		{int_type __C;
		if (_Sbuf == 0
			|| traits_type::eq_int_type(traits_type::eof(),
				__C = _Sbuf->sgetc()))
			_Sbuf = 0;
		else
			_Val = traits_type::to_char_type(__C);
		_Got = true;
		return (_Val); }
	streambuf_type *_Sbuf;
	bool _Got;
	_E _Val;
	};
template<class _E, class _Tr> inline
	bool operator==(const istreambuf_iterator<_E, _Tr>& __X,
		const istreambuf_iterator<_E, _Tr>& _Y)
	{return (__X.equal(_Y)); }
template<class _E, class _Tr> inline
	bool operator!=(const istreambuf_iterator<_E, _Tr>& __X,
		const istreambuf_iterator<_E, _Tr>& _Y)
	{return (!(__X == _Y)); }
		// TEMPLATE CLASS ostreambuf_iterator
template<class _E, class _Tr>
	class ostreambuf_iterator
		: public _Outit {
	typedef ostreambuf_iterator<_E, _Tr> _Myt;
public:
	typedef char char_type;
	typedef char_traits traits_type;
	typedef streambuf streambuf_type;
	typedef ostream ostream_type;
	ostreambuf_iterator(streambuf_type *_Sb) _THROW0()
		: _Failed(false), _Sbuf(_Sb) {}
	ostreambuf_iterator(ostream_type& _O) _THROW0()
		: _Failed(false), _Sbuf(_O.rdbuf()) {}
	_Myt& operator=(_E __X)
		{if (_Sbuf == 0
			|| traits_type::eq_int_type(_Tr::eof(),
				_Sbuf->sputc(__X)))
			_Failed = true;
		return (*this); }
	_Myt& operator*()
		{return (*this); }
	_Myt& operator++()
		{return (*this); }
	_Myt& operator++(int)
		{return (*this); }
	bool failed() const _THROW0()
		{return (_Failed); }
private:
	bool _Failed;
	streambuf_type *_Sbuf;
	};

//	ALGORITHM STUFF (from <algorithm>)
		// TEMPLATE FUNCTION copy
template<class _II, class _OI> inline
	_OI copy(_II _F, _II __L, _OI __X)
	{for (; _F != __L; ++__X, ++_F)
		*__X = *_F;
	return (__X); }
		// TEMPLATE FUNCTION copy_backward
template<class _BI1, class _BI2> inline
	_BI2 copy_backward(_BI1 _F, _BI1 __L, _BI2 __X)
	{while (_F != __L)
		*--__X = *--__L;
	return (__X); }
		// TEMPLATE FUNCTION equal
template<class _II1, class _II2> inline
	bool equal(_II1 _F, _II1 __L, _II2 __X)
	{return (mismatch(_F, __L, __X).first == __L); }
		// TEMPLATE FUNCTION equal WITH PRED
template<class _II1, class _II2, class _Pr> inline
	bool equal(_II1 _F, _II1 __L, _II2 __X, _Pr __P)
	{return (mismatch(_F, __L, __X, __P).first == __L); }
		// TEMPLATE FUNCTION fill
template<class _FI, class _Ty> inline
	void fill(_FI _F, _FI __L, const _Ty& __X)
	{for (; _F != __L; ++_F)
		*_F = __X; }
		// TEMPLATE FUNCTION fill_n
template<class _OI, class _Sz, class _Ty> inline
	void fill_n(_OI _F, _Sz __N, const _Ty& __X)
	{for (; 0 < __N; --__N, ++_F)
		*_F = __X; }
		// TEMPLATE FUNCTION lexicographical_compare
template<class _II1, class _II2> inline
	bool lexicographical_compare(_II1 _F1, _II1 _L1,
		_II2 _F2, _II2 _L2)
	{for (; _F1 != _L1 && _F2 != _L2; ++_F1, ++_F2)
		if (*_F1 < *_F2)
			return (true);
		else if (*_F2 < *_F1)
			return (false);
	return (_F1 == _L1 && _F2 != _L2); }
		// TEMPLATE FUNCTION lexicographical_compare WITH PRED
template<class _II1, class _II2, class _Pr> inline
	bool lexicographical_compare(_II1 _F1, _II1 _L1,
		_II2 _F2, _II2 _L2, _Pr __P)
	{for (; _F1 != _L1 && _F2 != _L2; ++_F1, ++_F2)
		if (__P(*_F1, *_F2))
			return (true);
		else if (__P(*_F2, *_F1))
			return (false);
	return (_F1 == _L1 && _F2 != _L2); }
		// TEMPLATE FUNCTION max
#ifndef _MAX
 #define _MAX	(max)
 #define _MIN	(min)
#endif
template<class _Ty> inline
	const _Ty& _MAX(const _Ty& __X, const _Ty& _Y)
	{return (__X < _Y ? _Y : __X); }
		// TEMPLATE FUNCTION max WITH PRED
template<class _Ty, class _Pr> inline
	const _Ty& _MAX(const _Ty& __X, const _Ty& _Y, _Pr __P)
	{return (__P(__X, _Y) ? _Y : __X); }
		// TEMPLATE FUNCTION min
template<class _Ty> inline
	const _Ty& _MIN(const _Ty& __X, const _Ty& _Y)
	{return (_Y < __X ? _Y : __X); }
		// TEMPLATE FUNCTION min WITH PRED
template<class _Ty, class _Pr> inline
	const _Ty& _MIN(const _Ty& __X, const _Ty& _Y, _Pr __P)
	{return (__P(_Y, __X) ? _Y : __X); }
		// TEMPLATE FUNCTION mismatch
template<class _II1, class _II2> inline
	pair<_II1, _II2> mismatch(_II1 _F, _II1 __L, _II2 __X)
	{for (; _F != __L && *_F == *__X; ++_F, ++__X)
		;
	return (pair<_II1, _II2>(_F, __X)); }
		// TEMPLATE FUNCTION mismatch WITH PRED
template<class _II1, class _II2, class _Pr> inline
	pair<_II1, _II2> mismatch(_II1 _F, _II1 __L, _II2 __X, _Pr __P)
	{for (; _F != __L && __P(*_F, *__X); ++_F, ++__X)
		;
	return (pair<_II1, _II2>(_F, __X)); }
		// TEMPLATE FUNCTION swap
template<class _Ty> inline
	void swap(_Ty& __X, _Ty& _Y)
	{_Ty _Tmp = __X;
	__X = _Y, _Y = _Tmp; }
_STD_END


#endif /* _XUTILITY_ */

/*
 * Copyright (c) 1995 by P.J. Plauger.  ALL RIGHTS RESERVED.
 * Consult your license regarding permissions and restrictions.
 */

/*
 * This file is derived from software bearing the following
 * restrictions:
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this
 * software and its documentation for any purpose is hereby
 * granted without fee, provided that the above copyright notice
 * appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation.
 * Hewlett-Packard Company makes no representations about the
 * suitability of this software for any purpose. It is provided
 * "as is" without express or implied warranty.
V2.2:0414 */
