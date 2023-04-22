extern crate proc_macro;

use quote::quote;
use syn::{parse::*, parse_macro_input};

#[derive(Debug, Clone)]
struct StructInfo {
    vis: syn::Visibility,
    ident: syn::Ident,
    generics: syn::Generics,
    fields: syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
}

impl StructInfo {
    fn parse(input: syn::DeriveInput) -> Self {
        let syn::DeriveInput {
            vis,
            ident,
            generics,
            data,
            ..
        } = input;

        let fields = match data {
            syn::Data::Struct(struct_data) => match struct_data.fields {
                syn::Fields::Named(fields_named) => fields_named.named,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };

        Self {
            vis,
            ident,
            generics,
            fields,
        }
    }
}

#[derive(Debug, Clone)]
struct BuilderInfo {
    struct_info: StructInfo,

    vis: syn::Visibility,
    builder_ident: syn::Ident,
    generics: syn::Generics,
    fields: Vec<syn::Field>,
}

impl BuilderInfo {
    fn parse(struct_info: StructInfo) -> Self {
        let builder_name = format!("{}Builder", struct_info.ident.to_string());
        let builder_ident = syn::Ident::new(&builder_name, proc_macro2::Span::call_site());

        let builder_fields = struct_info
            .fields
            .iter()
            .map(|f| {
                let ty = &f.ty;

                syn::Field {
                    attrs: vec![],
                    vis: syn::Visibility::Inherited,
                    ident: f.ident.clone(),
                    ty: syn::parse_quote!(Option<#ty>),
                    ..f.clone()
                }
            })
            .collect::<Vec<_>>();

        Self {
            struct_info: struct_info.clone(),

            vis: struct_info.vis,
            builder_ident: builder_ident,
            generics: struct_info.generics,
            fields: builder_fields,
        }
    }

    fn quote(self) -> Result<proc_macro2::TokenStream> {
        let builder_declaration = self.builder_declaration();
        let builder_impl = self.builder_impl()?;

        Ok(quote! {
            #builder_declaration

            #builder_impl
        })
    }

    fn builder_declaration(&self) -> proc_macro2::TokenStream {
        let Self {
            ref vis,
            ref builder_ident,
            ref generics,
            ref fields,
            ..
        } = self;

        quote! {
            #vis struct #builder_ident #generics {
                #( #fields, )*
            }
        }
    }

    fn builder_impl(&self) -> Result<proc_macro2::TokenStream> {
        let Self {
            ref builder_ident,
            ref generics,
            ..
        } = self;

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let new_method = self.new_method();
        let build_method = self.build_method();
        let setters = self.setters()?.into_iter();

        Ok(quote! {
            impl #impl_generics #builder_ident #ty_generics #where_clause {
                #new_method
                #build_method

                #( #setters )*
            }
        })
    }

    fn new_method(&self) -> proc_macro2::TokenStream {
        let fields_idents = self.fields.iter().map(|f| &f.ident);

        quote! {
            pub fn new() -> Self {
                Self {
                    #( #fields_idents: None, )*
                }
            }
        }
    }

    fn build_method(&self) -> proc_macro2::TokenStream {
        let Self {
            ref struct_info,
            ref generics,
            ..
        } = self;

        let struct_ident = &struct_info.ident;
        let (_, ty_generics, _) = generics.split_for_impl();

        let build_fields = self.build_fields();

        quote! {
            pub fn build(self) -> Option<#struct_ident #ty_generics> {
                Some(#struct_ident {
                    #( #build_fields?, )*
                })
            }
        }
    }
    
    fn build_fields(&self) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
        self.fields.iter().map(|f| {
            let ident = &f.ident;
            quote!(#ident: self.#ident)
        })
    }

    fn setters(&self) -> Result<Vec<proc_macro2::TokenStream>> {
        self.fields
            .iter()
            .zip(self.struct_info.fields.iter())
            .map(|(f, sf)| push_setter(f, &sf.attrs).and_then(|s| s.quote()))
            .collect::<Result<Vec<_>>>()
    }
}

enum Setter {
    Field(FieldSetter),
    Element(ElementSetter),
}

impl Setter {
    fn field(field: syn::Field, name: String) -> Self {
        Self::Field(FieldSetter::new(field, name))
    }

    fn element(field: syn::Field, name: String) -> Self {
        Self::Element(ElementSetter::new(field, name))
    }

    fn quote(self) -> Result<proc_macro2::TokenStream> {
        match self {
            Self::Field(field_setter) => field_setter.quote(),
            Self::Element(element_setter) => element_setter.quote(),
        }
    }
}

struct FieldSetter {
    field: syn::Field,
    ident: syn::Ident,
}

impl FieldSetter {
    fn new(field: syn::Field, name: String) -> Self {
        Self {
            field,
            ident: syn::Ident::new(&name, proc_macro2::Span::call_site()),
        }
    }

    fn quote(self) -> Result<proc_macro2::TokenStream> {
        let FieldSetter { field, ident } = self;

        let field_ident = field.ident;
        let field_ty = unwrap_type("Option", &field.ty)?;

        Ok(quote! {
            pub fn #ident(mut self, #field_ident: #field_ty) -> Self {
                self.#field_ident = Some(#field_ident);

                self
            }
        })
    }
}

struct ElementSetter {
    field: syn::Field,
    ident: syn::Ident,
}

impl ElementSetter {
    fn new(field: syn::Field, name: String) -> Self {
        Self {
            field,
            ident: syn::Ident::new(&name, proc_macro2::Span::call_site()),
        }
    }

    fn quote(self) -> Result<proc_macro2::TokenStream> {
        let ElementSetter { field, ident } = self;

        let field_ident = field.ident;
        let field_ty = unwrap_type("Vec", unwrap_type("Option", &field.ty)?)?;

        Ok(quote! {
            pub fn #ident(mut self, #ident: #field_ty) -> Self {
                let field = &mut self.#field_ident;

                if let Some(f) = field {
                    f.push(#ident);
                } else {
                    *field = Some(vec![#ident]);
                }

                self
            }
        })
    }
}

fn push_setter(f: &syn::Field, attrs: &[syn::Attribute]) -> Result<Setter> {
    let mut is_field_setter = true;
    let mut name = f.ident.as_ref().unwrap().to_string();

    attrs
        .iter()
        .filter(|a| a.path().is_ident("setter"))
        .try_for_each(|a| {
            a.parse_nested_meta(|meta| {
                if meta.path.is_ident("name") {
                    let value = meta.value()?;
                    let s: syn::LitStr = value.parse()?;

                    if is_field_setter {
                        name = s.value();
                    }

                    return Ok(());
                }

                if meta.path.is_ident("each") {
                    let value = meta.value()?;
                    let s: syn::LitStr = value.parse()?;

                    is_field_setter = false;
                    name = s.value();

                    return Ok(());
                }

                Err(meta.error("unrecognised setter attribute"))
            })
        })?;

    if is_field_setter {
        Ok(Setter::field(f.clone(), name))
    } else {
        Ok(Setter::element(f.clone(), name))
    }
}

fn unwrap_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Result<&'a syn::Type> {
    match ty {
        syn::Type::Path(type_path) => {
            let path_segment = type_path
                .path
                .segments
                .iter()
                .find(|ps| ps.ident.to_string() == wrapper.to_string())
                .ok_or(Error::new_spanned(
                    ty,
                    format!(
                        "Expected `{wrapper}`, found `{}`",
                        type_path_to_string(type_path)
                    ),
                ))?;

            match path_segment.arguments {
                syn::PathArguments::AngleBracketed(ref angle_bracketed) => {
                    if angle_bracketed.args.len() > 1 {
                        return Err(Error::new_spanned(
                            ty,
                            format!("Expected `{wrapper}<T>`, found `{wrapper}<T1, T2, ..., Tn>`"),
                        ));
                    }

                    match angle_bracketed.args.first() {
                        Some(syn::GenericArgument::Type(ty)) => Ok(ty),
                        _ => Err(Error::new_spanned(
                            ty,
                            format!("Expected `{wrapper}<T>`, found `{wrapper}`"),
                        )),
                    }
                }
                syn::PathArguments::Parenthesized(_) => Err(Error::new_spanned(
                    ty,
                    format!("Expected `{wrapper}<T>`, found `{wrapper}(T1, T2, ..., Tn)`"),
                )),
                syn::PathArguments::None => Err(Error::new_spanned(
                    ty,
                    format!("Expected `{wrapper}<T>`, found `{wrapper}`"),
                )),
            }
        }
        _ => Err(Error::new_spanned(ty, "Expected type path")),
    }
}

fn type_path_to_string(type_path: &syn::TypePath) -> String {
    type_path
        .path
        .segments
        .iter()
        .map(|p| p.ident.to_string())
        .collect::<Vec<String>>()
        .join("::")
}

#[proc_macro_derive(Builder, attributes(setter))]
pub fn derive_builder_proc_macro(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let derive_input = parse_macro_input!(input as syn::DeriveInput);
    let struct_info = StructInfo::parse(derive_input);
    let builder_info = BuilderInfo::parse(struct_info);

    match builder_info.quote() {
        Ok(bi) => bi.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
